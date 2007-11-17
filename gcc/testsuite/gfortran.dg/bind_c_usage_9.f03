! { dg-do compile }
! PR fortran/34133
!
! The compiler should reject internal procedures with BIND(c) attribute.
!
subroutine foo() bind(c)
contains
  subroutine bar() bind (c) ! { dg-error "may not be specified for an internal" }
  end subroutine bar ! { dg-error "Expected label" }
end subroutine foo ! { dg-warning "Extension: CONTAINS statement" }

subroutine foo2() bind(c)
  use iso_c_binding
contains
  integer(c_int) function barbar() bind (c) ! { dg-error "may not be specified for an internal" }
  end function barbar ! { dg-error "Expecting END SUBROUTINE" }
end subroutine foo2 ! { dg-warning "Extension: CONTAINS statement" }

function one() bind(c)
  use iso_c_binding
  integer(c_int) :: one
  one = 1
contains
  integer(c_int) function two() bind (c) ! { dg-error "may not be specified for an internal" }
  end function two ! { dg-error "Expected label" }
end function one ! { dg-warning "Extension: CONTAINS statement" }

function one2() bind(c)
  use iso_c_binding
  integer(c_int) :: one2
  one2 = 1
contains
  subroutine three() bind (c) ! { dg-error "may not be specified for an internal" }
  end function three ! { dg-error "Expected label" }
end function one2 ! { dg-warning "Extension: CONTAINS statement" }

program main
  use iso_c_binding
  implicit none
contains
  subroutine test() bind(c) ! { dg-error "may not be specified for an internal" }
  end subroutine test ! { dg-error "Expecting END PROGRAM" }
  function test2() bind (c) ! { dg-error "may not be specified for an internal" }
  end function test2  ! { dg-error "Expecting END PROGRAM" }
end program main ! { dg-warning "Extension: CONTAINS statement" }
