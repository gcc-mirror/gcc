! { dg-do run }
! PR fortran/36476
!
IMPLICIT NONE
CHARACTER (len=*) MY_STRING(1:3), my_string_s
PARAMETER ( MY_STRING = (/ "A" , "B", "C" /) )
PARAMETER ( MY_STRING_S = "AB C" )
character(len=*), parameter :: str(2) = [ 'Ac','cc']
character(len=*), parameter :: str_s = 'Acc'

CHARACTER (kind=1,len=*) MY_STRING1(1:3), my_string_s1
PARAMETER ( MY_STRING1 = (/ "A" , "B", "C" /) )
PARAMETER ( MY_STRING_S1 = "AB C" )
character(kind=1,len=*), parameter :: str1(2) = [ 1_'Ac',1_'cc']
character(kind=1,len=*), parameter :: str_s1 = 'Acc'

CHARACTER (kind=4,len=*) MY_STRING4(1:3), my_string_s4
PARAMETER ( MY_STRING4 = (/ 4_"A" , 4_"B", 4_"C" /) )
PARAMETER ( MY_STRING_S4 = 4_"AB C" )
character(kind=4,len=*), parameter :: str4(2) = [ 4_'Ac',4_'cc']
character(kind=4,len=*), parameter :: str_s4 = 4_'Acc'

if(len(MY_STRING)   /= 1) STOP 1
if(    MY_STRING(1) /= "A" &
   .or.MY_STRING(2) /= "B" &
   .or.MY_STRING(3) /= "C") STOP 2
if(len(MY_STRING_s)  /= 4) STOP 3
if(MY_STRING_S /= "AB C") STOP 4
if(len(str)        /= 2) STOP 5
if(str(1) /= "Ac" .or. str(2) /=  "cc") STOP 6
if(len(str_s)      /= 3) STOP 7
if(str_s /= 'Acc') STOP 8

if(len(MY_STRING1) /= 1) STOP 9
if(    MY_STRING1(1) /= 1_"A" &
   .or.MY_STRING1(2) /= 1_"B" &
   .or.MY_STRING1(3) /= 1_"C") STOP 10
if(len(MY_STRING_s1)  /= 4) STOP 11
if(MY_STRING_S1 /= 1_"AB C") STOP 12
if(len(str1)       /= 2) STOP 13
if(str1(1) /= 1_"Ac" .or. str1(2) /=  1_"cc") STOP 14
if(len(str_s1)     /= 3) STOP 15
if(str_s1 /= 1_'Acc') STOP 16

if(len(MY_STRING4) /= 1) STOP 17
if(    MY_STRING4(1) /= 4_"A" &
   .or.MY_STRING4(2) /= 4_"B" &
   .or.MY_STRING4(3) /= 4_"C") STOP 18
if(len(MY_STRING_s4)  /= 4) STOP 19
if(MY_STRING_S4 /= 4_"AB C") STOP 20
if(len(str4)       /= 2) STOP 21
if(str4(1) /= 4_"Ac" .or. str4(2) /=  4_"cc") STOP 22
if(len(str_s4)     /= 3) STOP 23
if(str_s4 /= 4_'Acc') STOP 24
end
