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
if (any (a /= 7)) call abort()
a = 88
if (any (a /= 88)) call abort()

 block
   integer :: b[*] = 8494
   if (b /= 8494) call abort()
 end block

if (any (a /= 88)) call abort()
call test ()
end

subroutine test()
  real :: z[*] = sqrt(2.0)
  if (z /= sqrt(2.0)) call abort()
  call sub1()
contains
  subroutine sub1
    real :: r[4,*] = -1
    if (r /= -1) call abort
    r = 10
    if (r /= 10) call abort
  end subroutine sub1

  subroutine uncalled()
     integer :: not_refed[2:*] = 784
     if (not_refed /= 784) call abort()
  end subroutine uncalled
end subroutine test
