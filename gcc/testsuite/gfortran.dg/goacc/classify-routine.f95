! Check offloaded function's attributes and classification for OpenACC
! routine.

! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-optimized-omp" }
! { dg-additional-options "-fdump-tree-ompexp" }
! { dg-additional-options "-fdump-tree-oaccdevlow" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

subroutine ROUTINE
  !$acc routine worker
  integer, parameter :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer :: i

  call setup(a, b)

  !$acc loop ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
  do i = 0, n - 1
     c(i) = a(i) + b(i)
  end do
end subroutine ROUTINE

! Check the offloaded function's attributes.
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(0 1, 1 0, 1 0\\), omp declare target \\(worker\\)\\)\\)" 1 "ompexp" } }

! Check the offloaded function's classification and compute dimensions (will
! always be 1 x 1 x 1 for non-offloading compilation).
! { dg-final { scan-tree-dump-times "(?n)Function is OpenACC routine level 1" 1 "oaccdevlow" } }
! { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccdevlow" } }
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(0 1, 1 1, 1 1\\), omp declare target \\(worker\\)\\)\\)" 1 "oaccdevlow" } }
