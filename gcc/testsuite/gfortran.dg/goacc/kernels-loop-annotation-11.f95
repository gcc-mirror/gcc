! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-std=legacy" }
! { dg-do compile }

! Test that a loop with a random label in the body cannot be annotated.

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i
  real :: t

  t = 0.0

!$acc kernels

  goto 10

  do i = 1, 16
10  t = t + a(i) * b(i)  ! { dg-warning "Possible control transfer to label" }
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop private.* auto" 0 "original" } }
