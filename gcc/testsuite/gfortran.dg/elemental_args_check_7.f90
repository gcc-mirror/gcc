! { dg-do compile }
!
! PR fortran/55638
!
! Additionally, VALUE no INTENT is required (and only "intent(in)" allowed)
!

  elemental subroutine foo(x, y, z)
    integer, intent(inout) :: x
    integer, VALUE :: y
    integer, VALUE, intent(in) :: z
    x = y
  end subroutine foo

  impure elemental subroutine foo2(x, y, z) ! { dg-error "Argument 'x' of elemental procedure 'foo2' at .1. must have its INTENT specified or have the VALUE attribute" }
    integer :: x 
    integer, VALUE :: y
    integer, VALUE :: z
    x = y
  end subroutine foo2

  subroutine foo3(x, y, z)
    integer, VALUE, intent(in) :: x
    integer, VALUE, intent(inout) :: y ! { dg-error "VALUE attribute conflicts with INTENT.INOUT. attribute" }
    integer, VALUE, intent(out) :: z ! { dg-error "VALUE attribute conflicts with INTENT.OUT. attribute" }
  end subroutine foo3
