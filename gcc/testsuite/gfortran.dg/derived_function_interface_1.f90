! { dg-do compile }
! Tests the fix for PR29634, in which an ICE would occur in the
! interface declaration of a function with an 'old-style' type
! declaration.  When fixed, it was found that the error message
! was not very helpful - this was fixed.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
module kinds
  type foo
    integer :: i
  end type foo
end module

type(foo) function ext_fun()
  use kinds
  ext_fun%i = 1
end function ext_fun

  use kinds

  interface fun_interface
    type(foo) function fun()
      use kinds
    end function fun
  end interface

  interface ext_fun_interface
    type(foo) function ext_fun()
      use kinds
    end function ext_fun
  end interface

  type(foo) :: x

  x = ext_fun ()
  print *, x%i

contains

  type(foo) function fun() ! { dg-error "already has an explicit interface" }
  end function fun  ! { dg-error "Expecting END PROGRAM" }

end ! { dg-warning "CONTAINS statement without FUNCTION or SUBROUTINE statement" }
! { dg-final { cleanup-modules "kinds" } }
