! { dg-do compile }
! One of the tests of the patch for PR30068.
! Taken from the fortran 2003 standard C11.2.
!
! The standard specifies that the optional arguments should be
! ignored in the counting of like type/kind, so the specific
! procedures below are invalid, even though actually unambiguous.
!
INTERFACE BAD8
  SUBROUTINE S8A(X,Y,Z)
    REAL,OPTIONAL :: X
    INTEGER :: Y
    REAL :: Z
  END SUBROUTINE S8A
  SUBROUTINE S8B(X,Z,Y)
    INTEGER,OPTIONAL :: X
    INTEGER :: Z
    REAL :: Y
  END SUBROUTINE S8B ! { dg-error "Ambiguous interfaces" }
END INTERFACE BAD8
real :: a, b
integer :: i, j
call bad8(x,i,b)
end
