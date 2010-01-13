! { dg-lto-do link }
! { dg-lto-options {{ -O1 -flto }} }
! { dg-suppress-ld-options "-O1" }

      SUBROUTINE ylm4(ylm)
      COMPLEX, INTENT (OUT):: ylm(1)
      INTEGER l,m
      COMPLEX ylms
      REAL, ALLOCATABLE, SAVE  :: ynorm(:)
      ylms = 0
      DO m = 1, 1
         DO l = m, 1
            ylm(m) = conjg(ylms)*ynorm(m)
         ENDDO
      ENDDO
      END SUBROUTINE ylm4

      PROGRAM test
      END
