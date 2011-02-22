! { dg-do compile }
!
! PR fortran/35779 - unrelated error message
! Tescase contributed by
! Dick Hendrickson <dick DOT hendrickson AT gmail DOT com>
!
! Initial patch was reverted as it broke nested loops (see initialization_26.f90).
!

!   INTEGER :: J1
!   INTEGER,PARAMETER :: I2(10) = (/(J1,J1=its_bad,1,-1)/) ! { dg - error "does not reduce" }
END
