subroutine f1 (a)
  integer :: a(*)
  integer i
  !$omp do order				! { dg-error "Failed to match clause" }
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do simd order :			! { dg-error "Failed to match clause" }
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp simd order ( foobar )		! { dg-error "Expected ORDER\\(CONCURRENT\\) at .1. with optional 'reproducible' or 'unconstrained' modifier" }
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do simd order( concurrent	! { dg-error "Expected ORDER\\(CONCURRENT\\) at .1. with optional 'reproducible' or 'unconstrained' modifier" }
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do simd order( concurrent : foo )! { dg-error "Expected ORDER\\(CONCURRENT\\) at .1. with optional 'reproducible' or 'unconstrained' modifier" }
  do i = 1, 128
    a(i) = a(i) + 1
  end do
end

subroutine f2 (a)
  integer :: a(*)
  integer i
  !$omp teams
  !$omp distribute order(concurrent)
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp end teams
  !$omp taskloop order (concurrent)	! { dg-error "Failed to match clause" }
  do i = 1, 128
    a(i) = a(i) + 1
  end do
  !$omp do order(concurrent) ordered	! { dg-error "ORDER clause must not be used together with ORDERED" }
  do i = 1, 128
      !$omp ordered
      a(i) = a(i) + 1
      !$omp end ordered
  end do
  !$omp do ordered order(concurrent)	! { dg-error "ORDER clause must not be used together with ORDERED" }
  do i = 1, 128
      !$omp ordered
      a(i) = a(i) + 1
      !$omp end ordered
  end do
  !$omp do ordered (1) order(concurrent)	! { dg-error "ORDER clause must not be used together with ORDERED" }
  do i = 1, 128
      !$omp ordered depend (sink: i - 1)
      !$omp ordered depend (source)
  end do
  !$omp do order(concurrent)ordered (1)	! { dg-error "ORDER clause must not be used together with ORDERED" }
  do i = 1, 128
      !$omp ordered depend (sink: i - 1)
      !$omp ordered depend (source)
  end do
end
