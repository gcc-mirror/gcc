! { dg-do compile }
! { dg-options "-std=f2008 -fcoarray=single" }

! PR fortran/44602
! Check for compile-time errors with non-loop EXITs.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: bar(2)

  ! Must not exit CRITICAL.
  mycrit: CRITICAL
    EXIT mycrit ! { dg-error "leaves CRITICAL" }
  END CRITICAL mycrit

  ! CYCLE is only allowed for loops!
  myblock: BLOCK
    CYCLE myblock ! { dg-error "is not applicable to non-loop construct 'myblock'" }
  END BLOCK myblock

  ! Invalid construct.
  ! Thanks to Mikael Morin, mikael.morin@sfr.fr.
  baz: WHERE ([ .true., .true. ])
    bar = 0
    EXIT baz ! { dg-error "is not applicable to construct 'baz'" }
  END WHERE baz
END PROGRAM main
