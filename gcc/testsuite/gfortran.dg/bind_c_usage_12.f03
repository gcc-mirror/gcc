! { dg-do compile }
! { dg-options "-std=gnu" }
! PR fortran/34133
!
! bind(C,name="...") is invalid for dummy procedures
! and for internal procedures.
!
subroutine dummy1(a,b)
!  implicit none
  interface
    function b() bind(c,name="jakl") ! { dg-error "no binding name is allowed" }
!     use iso_c_binding
!     integer(c_int) :: b       
    end function b ! { dg-error "Expecting END INTERFACE" }
  end interface
  interface
    subroutine a() bind(c,name="") ! { dg-error "no binding name is allowed" }
    end subroutine a ! { dg-error "Expecting END INTERFACE" }
  end interface
end subroutine dummy1

subroutine internal()
  implicit none
contains
  subroutine int1() bind(c, name="jj") ! { dg-error "No binding name is allowed" }
  end subroutine int1 ! { dg-error "Expected label" }
end subroutine internal

subroutine internal1()
  use iso_c_binding
  implicit none
contains
  integer(c_int) function int2() bind(c, name="jjj") ! { dg-error "No binding name is allowed" }
  end function int2 ! { dg-error "Expecting END SUBROUTINE" }
end subroutine internal1

integer(c_int) function internal2()
  use iso_c_binding
  implicit none
  internal2 = 0
contains
  subroutine int1() bind(c, name="kk") ! { dg-error "No binding name is allowed" }
  end subroutine int1 ! { dg-error "Expecting END FUNCTION" }
end function internal2

integer(c_int) function internal3()
  use iso_c_binding
  implicit none
  internal3 = 0
contains
  integer(c_int) function int2() bind(c, name="kkk") ! { dg-error "No binding name is allowed" }
  end function int2 ! { dg-error "Expected label" }
end function internal3

program internal_prog
  use iso_c_binding
  implicit none
contains
  subroutine int1() bind(c, name="mm") ! { dg-error "No binding name is allowed" }
  end subroutine int1 ! { dg-error "Expecting END PROGRAM statement" }
  integer(c_int) function int2() bind(c, name="mmm") ! { dg-error "No binding name is allowed" }
  end function int2 ! { dg-error "Expecting END PROGRAM statement" } 
end program
