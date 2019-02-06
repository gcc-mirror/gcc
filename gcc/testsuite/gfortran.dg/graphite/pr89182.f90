! { dg-do compile }
! { dg-options "-O3 -fgraphite-identity --param max-completely-peeled-insns=8" }

MODULE hfx_contract_block
  INTEGER, PARAMETER :: dp=8
CONTAINS
  SUBROUTINE contract_block(mb_max,mc_max,kbc,ks_bc)
    REAL(KIND=dp) :: kbc(mb_max*mc_max), ks_bc
    CALL block_1_2_1_2(kbc,ks_bc)
    CALL block_1_2_1_3(kbc,ks_bc)
    CALL block_1_2_1_3(kbc,ks_bc)
  END SUBROUTINE contract_block
  SUBROUTINE block_1_2_1_2(kbc,ks_bc)
    REAL(KIND=dp) :: kbc(2*1), ks_bc
    DO mc = 1,2
       DO mb = 1,2
          kbc((mc-1)*2+mb) = ks_bc
       END DO
    END DO
  END SUBROUTINE block_1_2_1_2
  SUBROUTINE block_1_2_1_3(kbc,ks_bc)
    REAL(KIND=dp) :: kbc(2*1), ks_bc
    DO md = 1,3
       DO mc = 1,1
          DO mb = 1,2
             kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
       END DO
    END DO
  END SUBROUTINE block_1_2_1_3
END MODULE hfx_contract_block
