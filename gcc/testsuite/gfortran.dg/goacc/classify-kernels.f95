! Check offloaded function's attributes and classification for OpenACC
! kernels.

! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-optimized-omp" }
! { dg-additional-options "-fdump-tree-ompexp" }
! { dg-additional-options "-fdump-tree-parloops1-all" }
! { dg-additional-options "-fdump-tree-oaccdevlow" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

program main
  implicit none
  integer, parameter :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer :: i

  call setup(a, b)

  !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1)) ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do i = 0, n - 1
     c(i) = a(i) + b(i)
  end do
  !$acc end kernels
end program main

! Check the offloaded function's attributes.
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels, omp target entrypoint\\)\\)" 1 "ompexp" } }

! Check that exactly one OpenACC kernels construct is analyzed, and that it
! can be parallelized.
! { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops1" } }
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint\\)\\)" 1 "parloops1" } }
! { dg-final { scan-tree-dump-not "FAILED:" "parloops1" } }

! Check the offloaded function's classification and compute dimensions (will
! always be 1 x 1 x 1 for non-offloading compilation).
! { dg-final { scan-tree-dump-times "(?n)Function is parallelized OpenACC kernels offload" 1 "oaccdevlow" } }
! { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccdevlow" } }
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(1, 1, 1\\), oacc kernels parallelized, oacc function \\(, , \\), oacc kernels, omp target entrypoint\\)\\)" 1 "oaccdevlow" } }
