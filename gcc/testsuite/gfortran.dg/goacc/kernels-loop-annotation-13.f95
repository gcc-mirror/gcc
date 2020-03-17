! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that in a situation with nested loops, a problem that prevents
! annotation of the outer loop only still allows the inner loop to be
! annotated.

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i, j
  real :: t

  t = 0.0

!$acc kernels

  do i = 1, 16
    if (a(i) < 0) then
      exit  ! { dg-warning "Exit" }
    end if
    do j = 1, 16
      t = t + a(i) * b(j)
    end do
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop private.* auto" 1 "original" } }
