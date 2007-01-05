! This checks that the "z = y" assignment is not considered copyable, as the
! array is of a derived type containing allocatable components.  Hence, we
! we should expand the scalarized loop, which contains *two* memcpy calls.
! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }

  type :: a
    integer, allocatable :: i(:)
  end type a

  type :: b
    type (a), allocatable :: at(:)
  end type b

  type(b) :: y(2), z(2)

  z = y
end
! { dg-final { scan-tree-dump-times "memcpy" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
