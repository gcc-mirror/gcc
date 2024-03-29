! { dg-do compile }
! Test the fix for PR113885, where not only was there a gimplifier ICE
! for a derived type 't' with no components but, with a component, gfortran
! gave wrong results.
! Contributed by David Binderman  <dcb314@hotmail.com>
!
module types
  type t
   contains
     final :: finalize
  end type t
contains
  pure subroutine finalize(x)
    type(t), intent(inout) :: x
  end subroutine finalize
end module types

subroutine test1(x)
  use types
  interface
     elemental function elem(x)
       use types
       type(t), intent(in) :: x
       type(t) :: elem
     end function elem
  end interface
  type(t) :: x(:)
  x = elem(x)
end subroutine test1

subroutine test2(x)
  use types
  interface
     elemental function elem(x)
       use types
       type(t), intent(in) :: x
       type(t) :: elem
     end function elem
     elemental function elem2(x, y)
       use types
       type(t), intent(in) :: x, y
       type(t) :: elem2
     end function elem2
  end interface
  type(t) :: x(:)
  x = elem2(elem(x), elem(x))
end subroutine test2
