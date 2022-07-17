! { dg-do compile }
! { dg-options "-fopenmp" }

module m
implicit none
integer :: i

contains

subroutine foo (x, y)
  integer :: x, y

  !$omp simd linear (x: step (y + 1), ref)		! { dg-error "LINEAR clause modifier other than VAL used on DO or SIMD construct" }
  do i = 0, 10
    x = x + y + 1
  end do

  !$omp simd linear (x: uval, step (y + 1))		! { dg-error "LINEAR clause modifier other than VAL used on DO or SIMD construct" }
  do i = 0, 10
    x = x + y + 1
  end do

  !$omp parallel do linear (x: ref, step (y + 1))	! { dg-error "LINEAR clause modifier other than VAL used on DO or SIMD construct" }
  do i = 0, 10
    x = x + y + 1
  end do

  !$omp parallel do linear (x: step (y + 1), uval)	! { dg-error "LINEAR clause modifier other than VAL used on DO or SIMD construct" }
  do i = 0, 10
    x = x + y + 1
  end do

  !$omp parallel do simd linear (x: step (y + 1), ref)	! { dg-error "LINEAR clause modifier other than VAL used on DO or SIMD construct" }
  do i = 0, 10
    x = x + y + 1
  end do

  !$omp parallel do simd linear (x: uval, step (y + 1))	! { dg-error "LINEAR clause modifier other than VAL used on DO or SIMD construct" }
  do i = 0, 10
    x = x + y + 1
  end do
end
end
