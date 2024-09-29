! { dg-do compile }
!
! PR fortran/35779 - unrelated error message
! Testcase contributed by
! Dick Hendrickson <dick DOT hendrickson AT gmail DOT com>
!
! Initial patch was reverted as it broke nested loops (see initialization_26.f90).
! XFAIL is for PR35779

!   INTEGER :: J1
!   INTEGER,PARAMETER :: I2(10) = (/(J1,J1=its_bad,1,-1)/) ! { dg-error "does not reduce" "PR35779" { xfail *-*-* } }
END
