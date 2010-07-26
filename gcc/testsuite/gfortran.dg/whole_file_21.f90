! { dg-do link }
! PR fortran/40011
!
! Contributed by Joost VandeVondele
!
!
! Before no "one" function was generated with -fwhole-file.
!
!
SUBROUTINE one ( )
END SUBROUTINE one

SUBROUTINE two ( )
END SUBROUTINE two

MODULE mod
CONTAINS
  SUBROUTINE three ( )
    CALL two ( )
  END SUBROUTINE three
  SUBROUTINE four ( )
      CALL one ( )
  END SUBROUTINE four
END MODULE mod
END

! { dg-final { cleanup-modules "m" } }
