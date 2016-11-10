subroutine foo (x, y)
  integer :: i, x, y
  common /i/ i
  interface
    function bar (x, y)
      integer :: x, y, bar
      !$omp declare simd (bar) linear (ref (x) : 1) linear (uval (y))
    end function bar
  end interface
  !$omp simd linear (x : y + 1)
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp simd linear (val (x) : y + 1)	! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp simd linear (ref (x) : y + 1)	! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp simd linear (uval (x) : y + 1)	! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do linear (x : y + 1)
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do linear (val (x) : y + 1)	! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do linear (ref (x) : y + 1)	! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do linear (uval (x) : y + 1)	! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do simd linear (x : y + 1)
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do simd linear (val (x) : y + 1) ! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do simd linear (ref (x) : y + 1) ! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
  !$omp do simd linear (uval (x) : y + 1) ! { dg-error "LINEAR clause modifier used on DO or SIMD construct" }
  do i = 1, 10
    x = x + y + 1
  end do
end
