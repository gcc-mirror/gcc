! { dg-do compile }
! { dg-options "-finit-local-zero -finit-derived -fdump-tree-original" }
!
! Make sure -finit-derived initializes components of local derived type
! variables to zero with -finit-local-zero.
!

subroutine dummy(i1,r1,c1,l1,i2,r2,c2,l2)
  implicit none
  integer, intent(in) :: i1
  real, intent(in) :: r1
  character, intent(in) :: c1
  logical, intent(in) :: l1
  integer, intent(out) :: i2
  real, intent(out) :: r2
  character, intent(out) :: c2
  logical, intent(out) :: l2
end subroutine

type t2
  integer i2
  real r2
  character c2
  logical l2
end type

type t1
  logical l1
  real r1
  character c1
  integer i1
  type (t2) y
end type

type (t1) :: x

call dummy (x%i1, x%r1, x%c1, x%l1, x%y%i2, x%y%r2, x%y%c2, x%y%l2)

end

! We expect to see each component initialized exactly once in MAIN.
! NB. the "once" qualifier also tests that the dummy variables aren't
! given an extraneous initializer.
! { dg-final { scan-tree-dump-times "i1= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "r1= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "c1= *\"\"" 1 "original" } }
! { dg-final { scan-tree-dump-times "l1= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "i2= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "r2= *0" 1 "original" } }
! { dg-final { scan-tree-dump-times "c2= *\"\"" 1 "original" } }
! { dg-final { scan-tree-dump-times "l2= *0" 1 "original" } }
