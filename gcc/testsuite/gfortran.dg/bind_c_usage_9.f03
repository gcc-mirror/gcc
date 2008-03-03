! { dg-do compile }
! { dg-options "-std=f2003" }
! PR fortran/34133
!
! The compiler should reject internal procedures with BIND(c) attribute
! for Fortran 2003.
!
subroutine foo() bind(c)
contains
  subroutine bar() bind (c) ! { dg-error "may not be specified for an internal" }
  end subroutine bar ! { dg-error "Expected label" }
end subroutine foo ! { dg-error "Fortran 2008: CONTAINS statement" }

subroutine foo2() bind(c)
  use iso_c_binding
contains
  integer(c_int) function barbar() bind (c) ! { dg-error "may not be specified for an internal" }
  end function barbar ! { dg-error "Expecting END SUBROUTINE" }
end subroutine foo2 ! { dg-error "Fortran 2008: CONTAINS statement" }

function one() bind(c)
  use iso_c_binding
  integer(c_int) :: one
  one = 1
contains
  integer(c_int) function two() bind (c) ! { dg-error "may not be specified for an internal" }
  end function two ! { dg-error "Expected label" }
end function one ! { dg-error "Fortran 2008: CONTAINS statement" }

function one2() bind(c)
  use iso_c_binding
  integer(c_int) :: one2
  one2 = 1
contains
  subroutine three() bind (c) ! { dg-error "may not be specified for an internal" }
  end subroutine three ! { dg-error "Expecting END FUNCTION statement" }
end function one2 ! { dg-error "Fortran 2008: CONTAINS statement" }

program main
  use iso_c_binding
  implicit none
contains
  subroutine test() bind(c) ! { dg-error "may not be specified for an internal" }
  end subroutine test ! { dg-error "Expecting END PROGRAM" }
  integer(c_int) function test2() bind (c) ! { dg-error "may not be specified for an internal" }
  end function test2  ! { dg-error "Expecting END PROGRAM" }
end program main ! { dg-error "Fortran 2008: CONTAINS statement" }
