! { dg-additional-options "-fno-inline" }

      subroutine init(COEF1,FORM1,AA)
      double precision COEF1,X
      double complex FORM1
      double precision AA(4,4)
      COEF1=0
      FORM1=0
      AA=0
      end
      subroutine curr(HADCUR)
      double precision COEF1
      double complex HADCUR(4),FORM1
      double precision AA(4,4)
      call init(COEF1,FORM1,AA)
      do i = 1,4
         do j = 1,4
            HADCUR(I)=
     $         HADCUR(I)+CMPLX(COEF1)*FORM1*AA(I,J)
         end do
      end do
      end
      program test
        double complex HADCUR(4)
        hadcur=0
        call curr(hadcur)
      end
