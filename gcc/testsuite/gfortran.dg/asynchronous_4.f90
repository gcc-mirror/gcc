! { dg-do compile }
!
! PR 59228: ICE with assumed type and ASYNCHRONOUS
!
! Contributed by Valery Weber <valeryweber@hotmail.com>

  IMPLICIT NONE

  interface
     subroutine test(base)
       TYPE(*), ASYNCHRONOUS :: base
     end subroutine
  end interface

CONTAINS

  SUBROUTINE foo ( data )
    REAL, DIMENSION( : ), ASYNCHRONOUS :: data
    CALL test ( data )                ! { dg-error "Rank mismatch in argument" }
  END SUBROUTINE

END
