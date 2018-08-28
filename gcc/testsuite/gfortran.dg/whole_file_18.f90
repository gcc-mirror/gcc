! { dg-do compile }
! { dg-options "-Wno-unused-dummy-argument" }
!
! PR fortran/34260
!
      PROGRAM MAIN
      REAL A
      CALL SUB(A)             ! { dg-error "Explicit interface required" }
      END PROGRAM

      SUBROUTINE SUB(A,I)
      REAL :: A
      INTEGER, OPTIONAL :: I
      END SUBROUTINE
