! { dg-do compile }
! { dg-options "-fno-tree-loop-ivcanon -O1 -floop-interchange -fno-tree-ccp -fno-tree-ch -fipa-pta" }
! PR tree-optimization/90021

MODULE a
  INTEGER b
CONTAINS
  SUBROUTINE bar(c)
    REAL c(1)
    INTEGER g, d, e, f 
    DO g = 1,3
      DO f = 1,1
        DO e = 1,3
          DO d = 1,1
            c(f-1+d) = c(f-1+d)*b
          END DO
        END DO
      END DO
    END DO
  END
  END
