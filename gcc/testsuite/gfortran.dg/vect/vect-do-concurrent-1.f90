! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-additional-options "-O3 -fopt-info-vec-optimized" }

subroutine test(n, a, b, c)
  integer, value :: n
  real, contiguous,  pointer :: a(:), b(:), c(:)
  integer :: i
  do concurrent (i = 1:n)
    a(i) = b(i) + c(i)
  end do
end subroutine test

! { dg-message "loop vectorized" "" { target *-*-* } 0 }
! { dg-bogus " version" "" { target *-*-* } 0 }
! { dg-bogus " alias" "" { target *-*-* } 0 }
! { dg-final { cleanup-tree-dump "vect" } }
