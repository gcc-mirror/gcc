! { dg-do compile }
! PR fortran/104351
! Contributed by G.Steinmetz

program p
  implicit none
  type t
  end type
  type(t) :: f
contains
  real function f() result(z) ! { dg-error "has an explicit interface" }
    z = 0.0                   ! { dg-error "assignment" }
  end function f              ! { dg-error "Expecting END PROGRAM" }
end
