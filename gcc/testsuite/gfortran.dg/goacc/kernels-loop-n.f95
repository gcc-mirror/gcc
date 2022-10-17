! { dg-additional-options "-O2" }
! { dg-additional-options "-fdump-tree-parloops1-all" }
! { dg-additional-options "-fdump-tree-optimized" }

module test
contains
  subroutine foo(n)
    implicit none
    integer :: n
    integer, dimension (0:n-1) :: a, b, c
    integer                    :: i, ii
    do i = 0, n - 1
       a(i) = i * 2
    end do

    do i = 0, n -1
       b(i) = i * 4
    end do

    !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1)) 
    do ii = 0, n - 1
       c(ii) = a(ii) + b(ii)
    end do
    !$acc end kernels

    do i = 0, n - 1
       if (c(i) .ne. a(i) + b(i)) STOP 1
    end do

  end subroutine foo
end module test

! Check that only one loop is analyzed, and that it can be parallelized.
! { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } }
! TODO, PR70545.
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint, noclone\\)\\)" 1 "parloops1" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } }

! Check that the loop has been split off into a function.
! { dg-final { scan-tree-dump-times "(?n);; Function __test_MOD_foo._omp_fn.0 " 1 "optimized" } }
