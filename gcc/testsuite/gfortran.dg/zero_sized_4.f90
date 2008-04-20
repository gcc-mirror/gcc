! { dg-do run }
! PR35991 run-time abort for CSHIFT of zero sized array
! Divide by zero exception before the patch.
      program try_gf0045
      call       gf0045(  9,  8)
      end

      subroutine GF0045(nf9,nf8)
      REAL RDA(10)
      REAL RDA1(0)

      RDA(NF9:NF8) = CSHIFT ( RDA1 ,1)

      END SUBROUTINE
