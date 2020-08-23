! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that an explicit annotation on an inner loop suppresses annotation
! of the outer loop, and produces a diagnostic.

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i, j
  real :: t

  t = 0.0

!$acc kernels

  do i = 1, 16
    !$acc loop seq  ! { dg-warning "Explicit loop annotation" }
    do j = 1, 16
      t = t + a(i) * b(j)
    end do
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop auto" 0 "original" } }
