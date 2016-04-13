! { dg-additional-options "-O2" }
! { dg-additional-options "-fdump-tree-ealias-all" }

program main
  implicit none
  integer, parameter :: n = 2
  integer  :: a, b, c, d

  !$acc kernels copyin (a) create (b) copyout (c) copy (d)
  a = 0
  b = 0
  c = 0
  d = 0
  !$acc end kernels

end program main

! { dg-final { scan-tree-dump-times "clique 1 base 1" 4 "ealias" } }
! { dg-final { scan-tree-dump-times "clique 1 base 2" 1 "ealias" } }
! { dg-final { scan-tree-dump-times "clique 1 base 3" 1 "ealias" } }
! { dg-final { scan-tree-dump-times "clique 1 base 4" 1 "ealias" } }
! { dg-final { scan-tree-dump-times "clique 1 base 5" 1 "ealias" } }
! { dg-final { scan-tree-dump-times "(?n)clique .* base .*" 8 "ealias" } }
