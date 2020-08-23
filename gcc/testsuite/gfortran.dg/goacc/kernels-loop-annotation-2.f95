! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that a loop with a variable bound can be annotated. 

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (:) :: a, b

  integer :: i, n
  real :: t

  t = 0.0
  n = size (a)

!$acc kernels

  do i = 1, n
    t = t + a(i) * b(i)
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop auto" 1 "original" } }
