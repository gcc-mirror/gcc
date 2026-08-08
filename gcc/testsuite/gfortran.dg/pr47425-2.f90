! { dg-do compile }
! PR47425 Test case from original report
subroutine sub1(L,s,e)
   implicit none
   character(*) L
   integer s,e
   if(any(L(s:e+1) == [character(len(L(s:e))+1)::'that','those'])) then
   end if
end subroutine sub1
