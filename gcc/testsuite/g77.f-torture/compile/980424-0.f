C     crashes in subst_stack_regs_pat on x86-linux, in the "abort();"
C     within the switch statement.
      SUBROUTINE C(A)
      COMPLEX A
      WRITE(*,*) A.NE.CMPLX(0.0D0)
      END
