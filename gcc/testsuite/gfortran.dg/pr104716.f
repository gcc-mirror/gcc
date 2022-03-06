! { dg-do compile }
! { dg-options "-std=legacy -O2 -ftree-loop-distribution -fno-move-loop-stores -fno-tree-dominator-opts" }

      SUBROUTINE FOO()
      
      COMMON /WORK/ C2(2, 2)
      
      DIMENSION D11(2)

      EQUIVALENCE (D11(1), C2(1, 1))

      DO 40 I = 1, 2
         DO 30 J = 1, 2
            ASSIGN 10 TO ILBL
            IF (C2(J, I) .NE. 0.0) THEN
               ASSIGN 20 TO ILBL
            ENDIF
            GO TO ILBL
 10         CONTINUE
 20         CONTINUE
            C2(J, I) = C2(J, I) + 1
 30      CONTINUE
 40   CONTINUE

      DO 50 I = 1, 2
         PRINT 90, I
 50   CONTINUE
      
      RETURN
 90   FORMAT(I5)
      END
