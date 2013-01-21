! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/55854
!
! Contributed by Damian Rouson
!

  type foo
    class(*), allocatable :: x
  end type
contains
  subroutine bar(this)
    type(foo), intent(out) :: this
  end
end

! { dg-final { scan-tree-dump-times "foo.0.x._data = 0B;" 1 "original" } }
! { dg-final { scan-tree-dump-times "foo.0.x._vptr = .* &__vtab__STAR;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
