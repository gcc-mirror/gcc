! { dg-do compile }

! PR middle-end/127723

module m
  implicit none
  type t
  end type t
contains
subroutine f (n, a, d)
  integer :: i, n, a(*)
  real :: d(*)

  !$omp simd simdlen(scaled(integer,1))  ! { dg-error "Expected '\\:' at .1." }
  do i = 1, 1
    a(i) = a(i) + 5
  end do

  !$omp simd simdlen(scaled(integer + 3): 2)  ! { dg-error "Expected '\\)' at .1." }
  do i = 1, 1
    a(i) = a(i) + 5
  end do

  !$omp simd simdlen(scaled(4): 8)  ! { dg-error "Expected numerical type name at .1." }
  do i = 1, 1
    d(i) = 0.0
  end do

  !$omp simd simdlen(scaled(type(t)): 8) ! { dg-error "Expected numerical type name at .1." }
  do i = 1, 1
    d(i) = 0.0
  end do

  !$omp simd simdlen(scaled(real, n): 8)  ! { dg-error "Divisor expression at .1. of the SCALED modifier to SIMDLEN must be a positive scalar constant integer expression" }
  do i = 1, 1
    d(i) = 0.0
  end do

  !$omp simd simdlen(scaled(complex, 0): 8)  ! { dg-error "Divisor expression at .1. of the SCALED modifier to SIMDLEN must be a positive scalar constant integer expression" }
  do i = 1, 1
    d(i) = 0.0
  end do

  !$omp simd simdlen(scaled(complex, 1, 0): 8)  ! { dg-error "Expected '\\)' at .1." }
  do i = 1, 1
    d(i) = 0.0
  end do
end
end
