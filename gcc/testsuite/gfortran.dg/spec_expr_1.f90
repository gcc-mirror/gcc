! { dg-do compile }
! PR 20323
! We didn't verify that character length expressions are specification
! expressions.
function testpresent(arg) 
   integer, intent(in), optional :: arg 
   character(len=arg) :: s ! { dg-error "OPTIONAL" }
   logical :: testpresent 
 
   testpresent=.true. 
 
end function testpresent 
