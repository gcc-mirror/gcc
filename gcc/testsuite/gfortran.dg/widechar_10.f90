! { dg-do compile }
! { dg-options "-pedantic" }
! PR fortran/36534
CHARACTER (kind=4,len=*) MY_STRING4(1:3)
PARAMETER ( MY_STRING4 = (/ "A" , "B", "C" /) )
end
