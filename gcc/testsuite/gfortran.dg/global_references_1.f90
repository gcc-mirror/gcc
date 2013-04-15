! { dg-do compile }
! This program tests the patch for PRs 20881, 23308, 25538 & 25710
! Assembled from PRs by Paul Thomas  <pault@gcc.gnu.org>
module m
contains
  subroutine g(x)   ! Local entity
    REAL :: x
    x = 1.0
  end subroutine g
end module m
! Error only appears once but testsuite associates with both lines.
function f(x)       ! { dg-error "is already being used as a FUNCTION" }
  REAL :: f, x
  f = x
end function f

function g(x)       ! Global entity
  REAL :: g, x
  g = x

! PR25710==========================================================
! Lahey -2607-S: "SOURCE.F90", line 26: 
! Function 'f' cannot be referenced as a subroutine. The previous
! definition is in 'line 12'.

  call f(g) ! { dg-error "is already being used as a FUNCTION|Interface mismatch in global procedure" }
end function g
! Error only appears once but testsuite associates with both lines.
function h(x)       ! { dg-error "is already being used as a FUNCTION" }
  REAL :: h, x
  h = x
end function h

SUBROUTINE TT()
  CHARACTER(LEN=10), EXTERNAL :: j ! { dg-error "Return type mismatch" }
  CHARACTER(LEN=10)          :: T
! PR20881=========================================================== 
! Error only appears once but testsuite associates with both lines.
  T = j (1.0) ! { dg-error "is already being used as a SUBROUTINE" }
  print *, T
END SUBROUTINE TT

  use m             ! Main program
  real x
  integer a(10)

! PR23308===========================================================
! Lahey - 2604-S: "SOURCE.F90", line 52:
! The name 'foo' cannot be specified as both external procedure name
! and common block name. The previous appearance is in 'line 68'.
! Error only appears once but testsuite associates with both lines.
  common /foo/ a    ! { dg-error "is already being used as a COMMON" }

  call f (x)        ! OK - reference to local entity
  call g (x)        !             -ditto-

! PR25710===========================================================
! Lahey - 2607-S: "SOURCE.F90", line 62:
! Function 'h' cannot be referenced as a subroutine. The previous
! definition is in 'line 29'.

  call h (x) ! { dg-error "is already being used as a FUNCTION|Interface mismatch in global procedure" }

! PR23308===========================================================
! Lahey - 2521-S: "SOURCE.F90", line 68: Intrinsic procedure name or
! external procedure name same as common block name 'foo'.

  call foo () ! { dg-error "is already being used as a COMMON" }

contains
  SUBROUTINE f (x)  ! Local entity
    real x
    x = 2
  end SUBROUTINE f
end

! PR20881=========================================================== 
! Lahey - 2636-S: "SOURCE.F90", line 81:
! Subroutine 'j' is previously referenced as a function in 'line 39'.

SUBROUTINE j (x)    ! { dg-error "is already being used as a SUBROUTINE" }
  integer a(10)
  common /bar/ a    ! Global entity foo
  real x
  x = bar(1.0)      ! OK for local procedure to have common block name
contains
  function bar (x)
    real bar, x
    bar = 2.0*x
  end function bar
END SUBROUTINE j

! PR25538===========================================================
! would ICE with entry and procedure having same names.
  subroutine link2 (namef) ! { dg-error "is already being used as a SUBROUTINE" }
    entry link2 (nameg)    ! { dg-error "is already being used as a SUBROUTINE" }
    return
  end
