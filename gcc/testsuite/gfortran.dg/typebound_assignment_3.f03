! { dg-do compile }
!
! PR 49074: [OOP] Defined assignment w/ CLASS arrays: Incomplete error message
!
! Contribute by Jerry DeLisle <jvdelisle@gcc.gnu.org>

module foo

  type bar
  contains
    generic :: assignment (=) => assgn
    procedure :: assgn
  end type

contains

  elemental subroutine assgn (a, b)
    class (bar), intent (inout) :: a
    class (bar), intent (in) :: b
  end subroutine

end module


  use foo
  type (bar) :: foobar(2)
  foobar = bar()           ! { dg-error "currently not implemented" }
end

! { dg-final { cleanup-modules "foo" } }
