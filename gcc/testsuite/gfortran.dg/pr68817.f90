! { dg-do compile }
! { dg-options "-O3 -ffast-math" }
      SUBROUTINE TEST(A,B,C)
      DIMENSION B(3),C(1000,10)
      DO I = 1,3
         I3=I*3
         B(1) = B(1) + (C(K,I3+1)-A)
         B(3) = B(3) + (C(K,I3+3)-A)
      ENDDO
      END

