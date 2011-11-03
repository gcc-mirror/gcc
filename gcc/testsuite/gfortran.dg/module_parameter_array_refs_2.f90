! { dg-do compile }
! { dg-options "-O" }
! { dg-final { scan-assembler-not "i_am_optimized_away" } }
!
! PR fortran/50960
!
! PARAMETER arrays and derived types exists as static variables.
! Check that the their read-only nature is taken into account
! when optimizations are done.
!

module m
  integer, parameter :: PARA(*) = [1,2,3,4,5,6,7,8,9,10]
end module m

subroutine test()
use m
integer :: i
i = 1
if (para(i) /= 1) call i_am_optimized_away()
end

! { dg-final { cleanup-modules "m" } }
