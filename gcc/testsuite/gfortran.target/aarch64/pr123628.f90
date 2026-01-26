! { dg-do compile }
! { dg-options "-O2 -march=armv9-a -fdump-tree-ifcvt -w" }
! { dg-final { scan-tree-dump {.MASK_CALL \(__builtin_expf, } ifcvt } }
!GCC$ builtin (expf) attributes simd (notinbranch)
SUBROUTINE a(b)
   REAL, DIMENSION(:) :: b
   c: DO i = 1, d
   IF (e <= f) THEN
      g = EXP(h)
      r = g
      IF (r > s) THEN
         b(i) = t
      END IF
   END IF
   END DO c
END
