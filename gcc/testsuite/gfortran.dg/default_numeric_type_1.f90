! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! Tests the fix for PR 31222, in which the type of the arguments of abs
! and int below were not detected to be of default numeric type..
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
subroutine mysub1(a,b,mode,dis)
!  integer :: mode
!  real :: dis
  dimension a(abs(mode)),b(int(dis))
  print *, mod
  write (*,*) abs(mode), nint(dis)
end subroutine

program testprog
  call mysub1((/1.,2./),(/1.,2.,3./),-2, 3.2)
end
