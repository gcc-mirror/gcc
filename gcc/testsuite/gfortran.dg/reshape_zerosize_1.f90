!  { dg-do run }
!  PR 35960 - there was a run-time abort when the SHAPE argument to
!  RESHAPE was zero-sized.
!  Test case contributed by Dick Henderson.
      program try_gf1065


! fails on Windows XP
! gcc version 4.4.0 20080312 (experimental) [trunk revision 133139]


      call       gf1065(1,  2,  3,  4,  7,  8,  9)
      end

      SUBROUTINE GF1065(nf1,nf2,nf3,nf4,nf7,nf8,nf9)

      REAL RDA(10,9)
      REAL RCA1(90)
      integer ila(2)
      RDA(NF9:NF8, NF7:NF3) = RESHAPE(RCA1,(/0,0/), (/1.0/),(/2,1/))

      rDA(NF9:NF8, NF7:NF3) = RESHAPE(rCA1,(/0,0/),ORDER=(/2,1/))

      ILA(1) = 5
      ILA(2) = 0
      rDA(NF4:NF8, NF7:NF3) = RESHAPE(rcA1,ILA)

      RdA(NF4:NF8, NF7:NF3) = RESHAPE(RcA1,ILA,PAD=(/-1.0/))

      ILA(1) = 0
      ILA(2) = 5
      RdA(NF9:NF8,NF4:NF8)=RESHAPE(RcA1,ILA,(/-1.0/),(/NF2,NF1/))

      ILA(1) = 5
      ILA(2) = 0
      RdA(NF4:NF8, NF7:NF3) = RESHAPE(RcA1,ILA,ORDER=(/NF1,NF2/))


      END SUBROUTINE
