! { dg-additional-options "-O2" }
! { dg-additional-options "-fdump-tree-ealias-all" }

program main
  implicit none
  integer, parameter :: n = 2
  integer, target, dimension (0:n-1) :: a
  integer, pointer :: ptr(:)
  ptr => a

  !$acc kernels pcopyin (a, ptr(0:2))
  a(0) = 0
  ptr(0) = 1
  !$acc end kernels

end program main

! Only the omp_data_i related loads should be annotated with cliques.
! { dg-final { scan-tree-dump-times "clique 1 base 1" 4 "ealias" } }
! { dg-final { scan-tree-dump-times "(?n)clique 1 base 0" 5 "ealias" } }
