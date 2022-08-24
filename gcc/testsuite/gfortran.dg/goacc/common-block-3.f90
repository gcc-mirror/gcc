! { dg-options "-fopenacc -fdump-tree-omplower" }

! { dg-additional-options "--param=openacc-kernels=decompose" }

! { dg-additional-options "-fopt-info-omp-all" }

! { dg-additional-options "--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

module consts
  integer, parameter :: n = 100
end module consts

program main
  use consts
  implicit none

  integer :: i, j
  real ::  a(n) = 0, b(n) = 0, c, d, e(n)
  real ::  x(n) = 0, y(n), z
  common /BLOCK/ a, b, c, j, d
  common /KERNELS_BLOCK/ x, y, z

  c = 1.0
  !$acc parallel loop copy(/BLOCK/) ! { dg-line l1 }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l1 }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l1 }
  do i = 1, n
     a(i) = b(i) + c
  end do
  !$acc kernels ! { dg-line l2 }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l2 }
  !   { dg-note {variable 'i' made addressable} {} { target *-*-* } l2 }
  ! { dg-note {variable 'c\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l2 }
  ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l2 }
  ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  do i = 1, n
     x(i) = y(i) + c
  end do
  !$acc end kernels
end program main

! { dg-final { scan-tree-dump-times "omp target oacc_parallel .*map\\(tofrom:a \\\[len: 400\\\]\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "omp target oacc_parallel .*map\\(tofrom:b \\\[len: 400\\\]\\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "omp target oacc_parallel .*map\\(tofrom:c \\\[len: 4\\\]\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "omp target oacc_data_kernels .*map\\(force_tofrom:i \\\[len: 4\\\]\\)" 1 "omplower" } }
!   { dg-final { scan-tree-dump-times "omp target oacc_kernels .*map\\(force_present:i \\\[len: 4\\\]\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "omp target oacc_data_kernels .*map\\(tofrom:x \\\[len: 400\\\]\\)" 1 "omplower" } }
!   { dg-final { scan-tree-dump-times "omp target oacc_kernels .*map\\(force_present:x \\\[len: 400\\\]\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "omp target oacc_data_kernels .*map\\(tofrom:y \\\[len: 400\\\]\\\)" 1 "omplower" } }
!   { dg-final { scan-tree-dump-times "omp target oacc_kernels .*map\\(force_present:y \\\[len: 400\\\]\\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "omp target oacc_data_kernels .*map\\(force_tofrom:c \\\[len: 4\\\]\\)" 1 "omplower" } }
!   { dg-final { scan-tree-dump-times "omp target oacc_kernels .*map\\(force_present:c \\\[len: 4\\\]\\)" 1 "omplower" } }

! Expecting no mapping of un-referenced common-blocks variables

! { dg-final { scan-tree-dump-not "map\\(.*:block" "omplower" } }
! { dg-final { scan-tree-dump-not "map\\(.*:kernels_block" "omplower" } }
! { dg-final { scan-tree-dump-not "map\\(.*:d " "omplower" } }
! { dg-final { scan-tree-dump-not "map\\(.*:e " "omplower" } }
! { dg-final { scan-tree-dump-not "map\\(.*:z " "omplower" } }
