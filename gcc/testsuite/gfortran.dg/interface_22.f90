! { dg-do compile }
!
! This is a check for error recovery: we used to ICE in various places, or
! emit bogus error messages (PR 25252)
!
module foo
  interface bar
    module procedure X, Y, ! { dg-error "Syntax error in MODULE PROCEDURE statement" }
  end interface bar
end module

module g
  interface i
    module procedure sint => sreal ! { dg-error "Syntax error in MODULE PROCEDURE statement" }
  end interface i
end module g

module gswap
  type points
    real :: x, y
  end type points
  interface swap
    module procedure sreal, schar, sint => sreal ! { dg-error "Syntax error in MODULE PROCEDURE statement" }
  end interface swap
end module gswap

! { dg-final { cleanup-modules "foo g gswap" } }

