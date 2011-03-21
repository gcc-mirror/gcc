! { dg-do compile }
! { dg-options "-fwhole-file -Wno-unused-dummy-argument" }
!
! PR fortran/34260
!
      PROGRAM MAIN
      REAL A
      CALL SUB(A)             ! { dg-error "requires an explicit interface" }
      END PROGRAM

      SUBROUTINE SUB(A,I)
      REAL :: A
      INTEGER, OPTIONAL :: I
      END SUBROUTINE
