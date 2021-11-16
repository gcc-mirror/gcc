! Check offloaded function's attributes and classification for OpenACC
! kernels.

! { dg-additional-options "--param openacc-kernels=decompose" }

! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-optimized" }
! { dg-additional-options "-fdump-tree-oaccloops1" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

program main
  implicit none
  integer, parameter :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer :: i

  call setup(a, b)

  !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1)) ! { dg-line l_compute1 }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute1 } */
  !   { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute1 } */
  do i = 0, n - 1 ! { dg-line l_loop1 }
    ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop1 }
    ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } l_loop1 }
    c(i) = a(i) + b(i)
  end do
  !$acc end kernels
end program main

! Check the offloaded function's classification and compute dimensions (will
! always be 1 x 1 x 1 for non-offloading compilation).
! { dg-final { scan-tree-dump-times "(?n)Function is parallel_kernels_graphite OpenACC kernels offload" 1 "oaccloops1" } }
! { dg-final { scan-tree-dump-times "(?n)Compute dimensions \\\[1, 1, 1\\\]" 1 "oaccloops1" } }
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(1, 1, 1\\), oacc parallel_kernels_graphite, omp target entrypoint, noclone\\)\\)" 1 "oaccloops1" } }
