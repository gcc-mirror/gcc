! { dg-do run }
! PR 35988 - failure on some zero-sized matmuls.
! Test case contributed by Dick Hendrickson.

       program try_gf1003

      call       gf1003a(  9,  8,  6)   
      call       gf1003b(  9,  8,  6)   
      call       gf1003c(  9,  8,  6)   !fails
      call       gf1003d(  9,  8,  6)   !fails
      end program


      SUBROUTINE GF1003a(nf9,nf8,nf6)
      REAL RDA(3,2)
      REAL RDA1(3,5)
      REAL RDA2(5,2)
      RDA = MATMUL(RDA1(:, 9:8),RDA2( 8:6,:))
      END SUBROUTINE

      SUBROUTINE GF1003b(nf9,nf8,nf6)
      REAL RDA(3,2)
      REAL RDA1(3,0)
      REAL RDA2(0,2)
      RDA = MATMUL(RDA1(:,NF9:NF8),RDA2(NF9:NF8,:))
      END SUBROUTINE

      SUBROUTINE GF1003c(nf9,nf8,nf6)
      REAL RDA(3,2)
      REAL RDA1(3,0)
      REAL RDA2(0,2)
      RDA = MATMUL(RDA1(:,NF9:NF8),RDA2(NF8:NF6,:))
      END SUBROUTINE

      SUBROUTINE GF1003d(nf9,nf8,nf6)
      REAL RDA(3,2)
      REAL RDA1(3,5)
      REAL RDA2(5,2)
      RDA = MATMUL(RDA1(:,NF9:NF8),RDA2(NF8:NF6,:))
      END SUBROUTINE
