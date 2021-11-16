! Check offloaded function's attributes and classification for unparallelized
! OpenACC kernels.

! { dg-additional-options "--param openacc-kernels=decompose" }

! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-note-optimized-omp" }
! { dg-additional-options "-fdump-tree-ompexp" }
! { dg-additional-options "-fdump-tree-graphite-all-details" }
! { dg-additional-options "-fdump-tree-oaccloops1-details" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

program main
  implicit none
  integer, parameter :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer :: i

  ! A function call in a data-reference makes the loop unparallelizable
  integer, external :: f

  call setup(a, b)

  !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1)) 
  do i = 0, n - 1 ! { dg-line l_loop1 }
    ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop1 }
    ! { dg-note {beginning 'Graphite' part in OpenACC 'kernels' region} {} { target *-*-* } l_loop1 }
    c(i) = a(f (i)) + b(f (i))
  end do
  !$acc end kernels
end program main

! Check the offloaded function's attributes.
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc parallel_kernels_graphite, omp target entrypoint, noclone\\)\\)" 1 "ompexp" } }

! Check the offloaded function's classification and compute dimensions (will
! always be 1 x 1 x 1 for non-offloading compilation).
! { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccloops1" } }
! { dg-final { scan-tree-dump-not "^assigned OpenACC.*?loop parallelism$" "oaccloops1" } }
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(1, 1, 1\\), oacc parallel_kernels_graphite, omp target entrypoint, noclone\\)\\)" 1 "oaccloops1" } }
