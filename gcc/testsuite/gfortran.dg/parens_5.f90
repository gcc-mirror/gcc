! { dg-do run }
! { dg-options "-std=legacy" }
!
! Another case of fallout from the original patch for PR14771
! Testcase by Erik Zeek
module para
contains
   function bobo(n)
       integer, intent(in) :: n
       character(len=(n)) :: bobo ! Used to fail here
       bobo = "1234567890"
   end function bobo
end module para

program test
   use para
   implicit none
   character*5 c
   c = bobo(5)
   if (c .ne. "12345") STOP 1
end program test
