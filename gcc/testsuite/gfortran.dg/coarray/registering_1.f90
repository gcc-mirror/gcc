! { dg-do run }
!
! PR fortran/18918
!
! Check whether registering coarrays works
!
module m
  integer :: a(1)[*] = 7
end module m

use m
if (any (a /= 7)) STOP 1
a = 88
if (any (a /= 88)) STOP 2

 block
   integer :: b[*] = 8494
   if (b /= 8494) STOP 3
 end block

if (any (a /= 88)) STOP 4
call test ()
end

subroutine test()
  real :: z[*] = sqrt(2.0)
  if (z /= sqrt(2.0)) STOP 5
  call sub1()
contains
  subroutine sub1
    real :: r[4,*] = -1
    if (r /= -1) STOP 1
    r = 10
    if (r /= 10) STOP 2
  end subroutine sub1

  subroutine uncalled()
     integer :: not_refed[2:*] = 784
     if (not_refed /= 784) STOP 6
  end subroutine uncalled
end subroutine test
