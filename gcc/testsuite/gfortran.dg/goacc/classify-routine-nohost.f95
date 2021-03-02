! Check offloaded function's attributes and classification for OpenACC
! routine with 'nohost' clause.

! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-optimized-omp" }
! { dg-additional-options "-fdump-tree-ompexp" }
! { dg-additional-options "-fdump-tree-oaccloops" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

subroutine ROUTINE
  !$acc routine nohost worker
  integer, parameter :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer :: i

  call setup(a, b)

  !$acc loop ! { dg-bogus "assigned OpenACC .* loop parallelism" }
  do i = 0, n - 1
     c(i) = a(i) + b(i)
  end do
end subroutine ROUTINE

! Check the offloaded function's attributes.
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(0 1, 1 0, 1 0\\), omp declare target \\(nohost worker\\)\\)\\)" 1 "ompexp" } }

! Check the offloaded function's classification.
! { dg-final { scan-tree-dump-times "(?n)Function is OpenACC routine level 1" 1 "oaccloops" } }
! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'routine' has 'nohost' clause" 1 "oaccloops" { target { ! offloading_enabled } } } }
! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'routine_' has 'nohost' clause" 1 "oaccloops" { target offloading_enabled } } }
! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'routine' discarded" 1 "oaccloops" { target { ! offloading_enabled } } } }
! { dg-final { scan-tree-dump-times "(?n)OpenACC routine 'routine_' discarded" 1 "oaccloops" { target offloading_enabled } } }
! { dg-final { scan-tree-dump-not "(?n)Compute dimensions" "oaccloops" } }
! { dg-final { scan-tree-dump-not "(?n)__attribute__\\(.*omp declare target \\(nohost" "oaccloops" } }
! { dg-final { scan-tree-dump-not "(?n)void routine \\(\\)" "oaccloops" { target { ! offloading_enabled } } } }
! { dg-final { scan-tree-dump-not "(?n)void routine_ \\(\\)" "oaccloops" { target offloading_enabled } } }
!TODO See PR101551 for 'offloading_enabled' differences.
