! { dg-do compile }
!
! PR fortran/35820
! 
! Memory leak(s) while resolving forall constructs.
! 
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>

      MODULE TESTS
      INTEGER,PARAMETER,PUBLIC  ::  I1_KV = KIND(1)
      INTEGER,PARAMETER,PUBLIC  ::  R1_KV = KIND(1.0)
      INTEGER, PRIVATE :: J1,J2
      INTEGER,PARAMETER,PUBLIC  ::  S1 = 10, S2 = 9
      CONTAINS
      SUBROUTINE SA0136(RDA,IDA,BDA)
      REAL(R1_KV) RDA(S1)
      INTEGER(I1_KV) IDA(S1,S2)
      INTEGER(I1_KV) ICA(S1,S2)
      REAL(R1_KV) RCA(S1)
!  T E S T  S T A T E M E N T S
      FORALL (J1 = 1:S1)
        RDA(J1) = RCA(J1) + 1.0_R1_KV
        FORALL (J2 = 1:S2)
          IDA(J1,J2) = ICA(J1,J2) + 1
        END FORALL
        FORALL (J2 = 1:S2)
          IDA(J1,J2) = ICA(J1,J2)
        END FORALL
      ENDFORALL
      FORALL (J1 = 1:S1)
        RDA(J1) = RCA(J1)
        FORALL (J2 = 1:S2)
          IDA(J1,J2) = ICA(J1,J2)
        END FORALL
      END FORALL
      END SUBROUTINE
      END MODULE TESTS
! { dg-final { cleanup-modules "tests" } }
