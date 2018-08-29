! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! Tests the fix for PR35944/6/7, in which the variable array constructors below
! were incorrectly translated and wrong code was produced.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
      program try_fa6013
      call fa6013 (10, 1, -1)
      call fa6077 (10, 1, -1, (/1,2,3,4,5,6,7,8,9,10/))
      call fa2083
      end program

      subroutine  FA6013 (nf10, nf1, mf1)
      integer, parameter :: kv = 4
      REAL(KV) DDA1(10)
      REAL(KV) DDA2(10)
      REAL(KV) DDA(10), dval
      dda = (/1,2,3,4,5,6,7,8,9,10/)
      DDA1 = ATAN2 ((/(REAL(J1,KV),J1=1,10)/),
     $                 REAL((/(J1,J1=nf10,nf1,mf1)/), KV))   !fails
      DDA2 = ATAN2 (DDA, DDA(10:1:-1))
      if (any (DDA1 - DDA2 .gt. epsilon(dval))) STOP 1
      END

      subroutine FA6077 (nf10,nf1,mf1, ida)
      INTEGER IDA1(10)
      INTEGER IDA2(10), ida(10)
      IDA1 = IEOR((/1,2,3,4,5,6,7,8,9,10/),
     $            (/(IDA(J1),J1=10,1,-1)/) )
      IDA2 = IEOR ((/1,2,3,4,5,6,7,8,9,10/), (/10,9,8,7,6,5,4,3,2,1/) )
      if (any (ida1 .ne. ida2)) STOP 2
      END SUBROUTINE

      subroutine fa2083
      implicit none
      integer j1,k
      parameter (k=selected_real_kind (precision (0.0_8) + 1)) ! failed
      REAL(k) QDA1(10)
      REAL(k) QDA(10), qval
      qda = (/ 1,2,3,4,5,6,7,8,9,10 /)
      QDA1 = MOD ( 1.1_k*( QDA(1) -5.0_k), P=( QDA -2.5_k))
      DO J1 = 1,10
        QVAL = MOD(1.1_k*(QDA(1)-5.0_k),P=(QDA(J1)-2.5_k))
        if (qval - qda1(j1) .gt. epsilon(qval)) STOP 3
      ENDDO
      END

