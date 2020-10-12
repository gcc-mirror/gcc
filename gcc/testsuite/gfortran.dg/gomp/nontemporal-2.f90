! { dg-do compile }

module m
 integer, allocatable :: a(:), b(:), c(:), d(:)
end module m

subroutine foo
  use m
  implicit none
  integer :: i

  !$omp simd nontemporal (a, b) aligned (a, b, c)
  do i = 1, ubound(a, dim=1)
    a(i) = b(i) + c(i)
  end do

  !$omp simd nontemporal (d) nontemporal (d)	! { dg-error "'d' present on multiple clauses" }
  do i = 1, ubound(d, dim=1)
    d(i) = 2 * c(i)
  end do

  !$omp simd nontemporal (a, b, b)		! { dg-error "'b' present on multiple clauses" }
  do i = 1, ubound(a, dim=1)
    a(i) = a(i) + b(i) + c(i)
  end do
end subroutine foo
