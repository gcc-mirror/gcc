! { dg-do compile }

      SUBROUTINE  SUB  (A,L,YMAX)
      DIMENSION A(L)
      YMA=A(1)
      DO 2 I=1,L,2
    2 YMA=MAX(YMA,A(I),A(I+1))
      CALL PROUND(YMA)
      END

! { dg-final { cleanup-tree-dump "vect" } }
