! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Ensure that for zero-sized array, nonzero memory is allocated
!
type t
end type t

type(t), allocatable :: x, y(:)

x = t()
y = [ t :: ]

if (.not. allocated (x)) call abort ()
if (.not. allocated (y)) call abort ()
end

! { dg-final { scan-tree-dump "x = \\(struct t .\\) __builtin_malloc \\(1\\);" "original" } }
! { dg-final { scan-tree-dump "y.data = \\(void . restrict\\) __builtin_malloc \\(1\\);" "original" } }
