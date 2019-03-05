! { dg-options "-O3 -fdump-tree-lversion-details" }

! Check that versioning is applied to a gather-like reduction operation.

function f(x, index, n)
  integer :: n
  real :: x(:)
  integer :: index(n)
  f = sum(x(index(:)))
end function f

! { dg-final { scan-tree-dump-times {want to version containing loop} 1 "lversion" } }
! { dg-final { scan-tree-dump-times {versioned this loop} 1 "lversion" } }
