open Array;;
exception Empty;;
exception AtLast;;
exception AtFirst;;
exception TooShort;;
let to_string l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;
type editable_string=(char array* int) ref;;
let create s=
  	let rec exp i l=
  	if i<0 then l else exp (i-1) (append [|s.[i]|] l) in
  	let l=exp (String.length s -1) [||] in
  	ref(l,0);;
let forward editable_string=
	let (arr,k)= !editable_string in
	if k>=(length arr -1) then raise AtLast
	else editable_string := (arr,k+1);;
let backward editable_string=
  	let (arr,k)= !editable_string in
  	if k<=0 then raise AtFirst
  	else editable_string :=(arr,k-1);;
let moveTo n editable_string=
  	let (arr,k)= !editable_string in
  	if n>=(length arr) then raise TooShort
  	else
  		editable_string:=(arr,n);;
let replace editable_string w n=
	let (arr,k)= !editable_string in
	arr.(n)<-w;;
let lgh editable_string=
	let(arr,k)= !editable_string in
	length arr;;
let nonempty editable_string =
  	let (arr,k) = !editable_string in
  	if (length arr)>0 then true else false;;
let concat editable_string_1 editable_string_2=
	let (arr_1,k_1)= !editable_string_1 in
	let (arr_2,k_2)= !editable_string_2 in
	let lis_1=to_list arr_1 in
	let lis_2=to_list arr_2 in
	let str1=to_string lis_1 in
	let str2=to_string lis_2 in
	str1^str2;;
let first editable_string=
	let (arr,k)= !editable_string in
	if (length arr)=0 then raise Empty
	else arr.(0);;
let last editable_string=
	let (arr,k)= !editable_string in
	if (length arr)=0 then raise Empty
	else arr.(length arr -1);;
let reverse editable_string=
	let (arr,k)= !editable_string in
	let b=copy arr in
	let len=length arr in
	for i=0 to len-1 do
		arr.(i)<-b.(len-i-1)
	done;
	let lis=to_list arr in
	let str=to_string lis in
	str;;


let alphabet=[|'1';'2';'a';'b';'c';'A'|];;
let w1=create "";;
let w2=create "a";;
let w3=create "abc";;
let w4=create "12";;

lgh w1;;
lgh w2;;
lgh w3;;
lgh w4;;

nonempty w1;;
nonempty w2;;
nonempty w4;;

concat w1 w1;;
concat w1 w2;;
concat (create "1") w1;;
concat (create "1A") w3;;

reverse w1;;
reverse w3;;
reverse w4;;

first w1;;
first w2;;
first w3;;

last w1;;
last w2;;
last w3;;

let editable=create "abac12a2aAac211";;
forward editable;;
backward editable;;
moveTo 10 editable;;
replace editable 'b' 2;;
