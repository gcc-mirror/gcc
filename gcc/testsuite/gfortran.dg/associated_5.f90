! { dg-do run }
! PR 35719 - associated used to fail with zero-sized automatic arrays
! Test case contributed by Dick Hendrickson

      program try_mf1053

      call       mf1053 (  1,   2,   3,   4)
      end

      SUBROUTINE MF1053 (nf1, nf2, nf3, nf4)
      INTEGER, pointer :: ptr(:,:)
      INTEGER, target  :: ILA1(NF2,NF4:NF3)

      ptr => ILA1

      if (ASSOCIATED (ptr, ILA1(NF1:NF2,NF4:NF3) ) ) call abort
      if ( .not. ASSOCIATED(ptr) )  call abort

      END SUBROUTINE
