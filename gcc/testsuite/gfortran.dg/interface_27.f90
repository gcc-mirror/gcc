! { dg-do compile }
!
! PR 40039: Procedures as actual arguments: Check intent of arguments
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

contains

subroutine a(x,f)
  real :: x
  interface
    real function f(y)
      real,intent(in) :: y
    end function
  end interface
  print *,f(x)
end subroutine

real function func(z)
  real,intent(inout) :: z
  func = z**2
end function

subroutine caller
  interface
    real function p(y)
      real,intent(in) :: y
    end function
  end interface
  pointer :: p

  call a(4.3,func)  ! { dg-error "INTENT mismatch in argument" }
  p => func         ! { dg-error "INTENT mismatch in argument" }
end subroutine

end module 
