! { dg-do compile }
!
! Check the fix for an ICE in gfc_conv_expr_present, which resulted from
! the rhs temporary picking up the optional attribute from the lhs in a
! defined assignment.
!
! Contributed by Jakub Jelenik  <jakub@gcc.gnu.org>
!
module foo
  type t1
  contains
    procedure bar
    generic :: assignment(=) => bar
  end type
  type t2
    type(t1) m
  end type
contains
  subroutine bar (x, y)
    intent(in) y
    class(t1), intent(out) :: x
  end subroutine
end module
subroutine baz (x, y)
  use foo
  integer y
  type(t2), pointer, optional :: x
  interface
    function qux (x)
      use foo
      integer x
      type(t2) qux
    end function
  end interface
  if (present (x)) then
    x = qux (y)  ! ICE was here
  end if
end subroutine
