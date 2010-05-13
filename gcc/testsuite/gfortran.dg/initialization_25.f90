! { dg-do "compile" }
!
! PR fortran/35779 - unrelated error message
! Tescase contributed by
! Dick Hendrickson <dick DOT hendrickson AT gmail DOT com>
!

  INTEGER :: J1
  INTEGER,PARAMETER :: I3(10) = (/(J1,J1=10,1,-1)/)
  INTEGER,PARAMETER :: I2(10) = (/(J1,J1=its_bad,1,-1)/) ! { dg-error "does not reduce" }
END
