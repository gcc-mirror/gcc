! { dg-additional-options "--param openacc-kernels=decompose" } as this is
! specifically testing "Graphite" handling.
! { dg-additional-options "-O2" }
! { dg-additional-options "-fdump-tree-graphite-details" }
! { dg-additional-options "-fdump-tree-optimized" }

program main
  implicit none
  integer, parameter         :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer                    :: i, ii

  !$acc kernels copyout (a(0:n-1))
  do i = 0, n - 1
     a(i) = i * 2
  end do
  !$acc end kernels

  !$acc kernels copyout (b(0:n-1))
  do i = 0, n -1
     b(i) = i * 4
  end do
  !$acc end kernels

  !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1))
  do ii = 0, n - 1
     c(ii) = a(ii) + b(ii)
  end do
  !$acc end kernels

  do i = 0, n - 1
     if (c(i) .ne. a(i) + b(i)) STOP 1
  end do

end program main

! { dg-final { scan-tree-dump-times "loop has no data-dependences" 6 "graphite" } } ! Two CFG loops per OpenACC loop
! { dg-final { scan-tree-dump-not "loop has data-dependences" "graphite" } }
! { dg-final { scan-tree-dump-times "(?n)__attribute__\\(\\(oacc function \\(, , \\), oacc parallel_kernels_graphite, omp target entrypoint, noclone\\)\\)" 3 "graphite" } }

! Check that the loop has been split off into a function.
! { dg-final { scan-tree-dump-times "(?n);; Function MAIN__._omp_fn.0 " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "(?n);; Function MAIN__._omp_fn.1 " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "(?n);; Function MAIN__._omp_fn.2 " 1 "optimized" } }
