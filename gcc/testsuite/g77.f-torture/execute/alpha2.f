c     This was originally a compile test.
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /C/   A(9), INT
      DATA A      /
     1                 0.49999973986348730D01, 0.40000399113084100D01,
     2                 0.29996921166596490D01, 0.20016917082678680D01,
     3                 0.99126390351864390D00, 0.97963256554443300D-01,
     4                -0.87360964813570100D-02, 0.16917082678692080D-02,
     5                7./
C     Data values were once mis-compiled on (OSF/1 ?) Alpha with -O2
c     such that, for instance, `7.' appeared as `4.' in the assembler
c     output.
      call test(a(9), 7)
      END
      subroutine test(r, i)
      double precision r
      if (nint(r)/=i) call abort
      end

