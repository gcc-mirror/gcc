! { dg-do run }
! PR 35990 - some empty array sections caused pack to crash.
! Test case contributed by Dick Hendrickson, adjusted and
! extended by Thomas Koenig.
      program try_gf1048

      call       gf1048a(  10,  8,  7,  1,  0,  .true.) 
      call       gf1048b(  10,  8,  7,  1,  0,  .true.) 
      call       gf1048c(  10,  8,  7,  1,  0,  .true.) 
      call       gf1048d(  10,  8,  7,  1,  0,  .true.) 
      call       P_inta (  10,  8,  7,  1,  0,  .true.)    
      call       P_intb (  10,  8,  7,  1,  0,  .true.)    
      call       P_intc (  10,  8,  7,  1,  0,  .true.)    
      call       P_intd (  10,  8,  7,  1,  0,  .true.)    
      end program

      SUBROUTINE GF1048a(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      CHARACTER(9) BDA(10)
      CHARACTER(9) BDA1(10)
      BDA(  8:7) = PACK(BDA1( 10:  1), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE GF1048b(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      CHARACTER(9) BDA(10)
      CHARACTER(9) BDA1(nf10)
      BDA(NF8:NF7) = PACK(BDA1(NF8:NF7), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE GF1048c(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      CHARACTER(9) BDA(10)
      CHARACTER(9) BDA1(10)
      BDA(NF8:NF7) = PACK(BDA1(NF10:NF1), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE GF1048d(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      CHARACTER(9) BDA(10)
      CHARACTER(9) BDA1(nf10)
      BDA(NF8:NF7) = PACK(BDA1(NF10:NF1), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE P_INTa(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      INTEGER BDA(10)
      INTEGER BDA1(10)
      BDA(  8:7) = PACK(BDA1( 10:  1), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE P_INTb(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      INTEGER BDA(10)
      INTEGER BDA1(nf10)
      BDA(NF8:NF7) = PACK(BDA1(NF8:NF7), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE P_INTc(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      INTEGER BDA(10)
      INTEGER BDA1(10)
      BDA(NF8:NF7) = PACK(BDA1(NF10:NF1), NF_TRUE)
      END SUBROUTINE

      SUBROUTINE P_INTd(nf10,nf8,nf7,nf1,nf0,nf_true)
      logical nf_true
      INTEGER BDA(10)
      INTEGER BDA1(nf10)
      BDA(NF8:NF7) = PACK(BDA1(NF10:NF1), NF_TRUE)
      END SUBROUTINE

