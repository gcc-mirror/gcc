! { dg-additional-options "-fopenacc -fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-Wopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fdump-tree-original" }
! { dg-do compile }

! Test that loops containing I/O statements can't be annotated.

function f (a, b)
  implicit none

  real :: f
  real, intent (in), dimension (16) :: a, b

  integer :: i, j
  real :: t

  t = 0.0

!$acc kernels

  do i = 1, 16
    do j = 1, 16
      print *, " i =", i, " j =", j  ! { dg-warning "I/O statement" }
      t = t + a(i) * b(j)
    end do
  end do

  f = t

!$acc end kernels

end function f

! { dg-final { scan-tree-dump-times "acc loop private.* auto" 0 "original" } }
