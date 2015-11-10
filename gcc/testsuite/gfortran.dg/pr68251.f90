! PR middle-end/68251
! Reduced testcase by Joost VandeVondele <Joost.VandeVondele@mat.ethz.ch>

! { dg-do compile }
! { dg-options "-O3" }

MODULE hfx_contract_block
  INTEGER, PARAMETER :: dp=8
CONTAINS
  SUBROUTINE contract_block(ma_max,mb_max,mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(mb_max*md_max), kbc(mb_max*mc_max), &
      kad(ma_max*md_max), kac(ma_max*mc_max), pbd(mb_max*md_max), &
      pbc(mb_max*mc_max), pad(ma_max*md_max), pac(ma_max*mc_max), &
      prim(ma_max*mb_max*mc_max*md_max), scale
    SELECT CASE(ma_max)
    CASE(1)
      SELECT CASE(mb_max)
      CASE(1)
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_1_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_1_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_1_11(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(2)
            CALL block_1_2_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_1_5(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_1_6(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_1_7(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_2_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_4_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_2_6_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_2_7_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_3_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_1_5(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_1_6(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_2_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_3_3_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_3_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_3_5(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_3_5(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_4_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_4_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_4_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_4_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_5_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_6_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_6_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_1_6_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_1_6_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          END SELECT
        END SELECT
      END SELECT
      SELECT CASE(mb_max)
      CASE(1)
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_2_1_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_1_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_1_1_5(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_1_1_6(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_1_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_1_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_1_2_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_2_2_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_2_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_2_2_3_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(7)
            CALL block_3_2_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_2_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_2_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_2_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_2_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_3_5_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
        END SELECT
        CALL block_3_9(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
      END SELECT
      SELECT CASE(mb_max)
      CASE(1)
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_4_1_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_4_2_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
            CALL block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          SELECT CASE(md_max)
          CASE(1)
            CALL block_4_3_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          CALL block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          SELECT CASE(md_max)
          CASE(1)
            CALL block_4_4_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
          END SELECT
        END SELECT
        SELECT CASE(mc_max)
        CASE(1)
          SELECT CASE(md_max)
          END SELECT
        END SELECT
      END SELECT
      SELECT CASE(mb_max)
      CASE(1)
        CALL block_15_15(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
      END SELECT
    END SELECT
  END SUBROUTINE contract_block
  SUBROUTINE block_1_1_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*1), kbc(1*1), kad(1*1), kac(1*1), pbd(1*1), &
      pbc(1*1), pad(1*1), pac(1*1), prim(1*1*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,1
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_1_1_1
  SUBROUTINE block_1_1_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*2), kbc(1*1), kad(1*2), kac(1*1), pbd(1*2), &
      pbc(1*1), pad(1*2), pac(1*1), prim(1*1*1*2), scale
      DO md = 1,2
        DO mc = 1,1
          DO mb = 1,1
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_1_1_2
  SUBROUTINE block_1_1_11(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*md_max), kbc(1*11), kad(1*md_max), kac(1*11), &
      pbd(1*md_max), pbc(1*11), pad(1*md_max), pac(1*11), &
      prim(1*1*11*md_max), scale
      DO md = 1,md_max
        DO mc = 1,11
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_1_11
  SUBROUTINE block_1_2_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*2), kbc(2*1), kad(1*2), kac(1*1), pbd(2*2), &
      pbc(2*1), pad(1*2), pac(1*1), prim(1*2*1*2), scale
      DO md = 1,2
        DO mc = 1,1
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_1_2
  SUBROUTINE block_1_2_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*3), kbc(2*1), kad(1*3), kac(1*1), pbd(2*3), &
      pbc(2*1), pad(1*3), pac(1*1), prim(1*2*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_1_3
  SUBROUTINE block_1_2_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*4), kbc(2*1), kad(1*4), kac(1*1), pbd(2*4), &
      pbc(2*1), pad(1*4), pac(1*1), prim(1*2*1*4), scale
      DO md = 1,4
        DO mc = 1,1
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_1_4
  SUBROUTINE block_1_2_1_5(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*5), kbc(2*1), kad(1*5), kac(1*1), pbd(2*5), &
      pbc(2*1), pad(1*5), pac(1*1), prim(1*2*1*5), scale
      DO md = 1,5
        DO mc = 1,1
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_1_5
  SUBROUTINE block_1_2_1_6(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*6), kbc(2*1), kad(1*6), kac(1*1), pbd(2*6), &
      pbc(2*1), pad(1*6), pac(1*1), prim(1*2*1*6), scale
      DO md = 1,6
        DO mc = 1,1
          DO mb = 1,2
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_1_6
  SUBROUTINE block_1_2_1_7(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*7), kbc(2*1), kad(1*7), kac(1*1), pbd(2*7), &
      pbc(2*1), pad(1*7), pac(1*1), prim(1*2*1*7), scale
      DO md = 1,7
        DO mc = 1,1
          DO mb = 1,2
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_1_7
  SUBROUTINE block_1_2_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*2), kbc(2*2), kad(1*2), kac(1*2), pbd(2*2), &
      pbc(2*2), pad(1*2), pac(1*2), prim(1*2*2*2), scale
      DO md = 1,2
        DO mc = 1,2
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_2_2
  SUBROUTINE block_1_2_2_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*4), kbc(2*2), kad(1*4), kac(1*2), pbd(2*4), &
      pbc(2*2), pad(1*4), pac(1*2), prim(1*2*2*4), scale
      DO md = 1,4
        DO mc = 1,2
          DO mb = 1,2
            kbd((md-1)*2+mb) = kbd((md-1)*2+mb) - ks_bd
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_2_4
  SUBROUTINE block_1_2_4_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*1), kbc(2*4), kad(1*1), kac(1*4), pbd(2*1), &
      pbc(2*4), pad(1*1), pac(1*4), prim(1*2*4*1), scale
      DO md = 1,1
        DO mc = 1,4
          DO mb = 1,2
            kbd((md-1)*2+mb) = kbd((md-1)*2+mb) - ks_bd
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_4_1
  SUBROUTINE block_1_2_6_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*1), kbc(2*6), kad(1*1), kac(1*6), pbd(2*1), &
      pbc(2*6), pad(1*1), pac(1*6), prim(1*2*6*1), scale
      DO md = 1,1
        DO mc = 1,6
          DO mb = 1,2
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_6_1
  SUBROUTINE block_1_2_7_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*1), kbc(2*7), kad(1*1), kac(1*7), pbd(2*1), &
      pbc(2*7), pad(1*1), pac(1*7), prim(1*2*7*1), scale
      DO md = 1,1
        DO mc = 1,7
          DO mb = 1,2
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_2_7_1
  SUBROUTINE block_1_3_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*1), kbc(3*1), kad(1*1), kac(1*1), pbd(3*1), &
      pbc(3*1), pad(1*1), pac(1*1), prim(1*3*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,3
            kbc((mc-1)*3+mb) = kbc((mc-1)*3+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_1_1
  SUBROUTINE block_1_3_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*3), kbc(3*1), kad(1*3), kac(1*1), pbd(3*3), &
      pbc(3*1), pad(1*3), pac(1*1), prim(1*3*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,3
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_1_3
  SUBROUTINE block_1_3_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*4), kbc(3*1), kad(1*4), kac(1*1), pbd(3*4), &
      pbc(3*1), pad(1*4), pac(1*1), prim(1*3*1*4), scale
      DO md = 1,4
        DO mc = 1,1
          DO mb = 1,3
            kbc((mc-1)*3+mb) = kbc((mc-1)*3+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_1_4
  SUBROUTINE block_1_3_1_5(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*5), kbc(3*1), kad(1*5), kac(1*1), pbd(3*5), &
      pbc(3*1), pad(1*5), pac(1*1), prim(1*3*1*5), scale
      DO md = 1,5
        DO mc = 1,1
          DO mb = 1,3
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_1_5
  SUBROUTINE block_1_3_1_6(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*6), kbc(3*1), kad(1*6), kac(1*1), pbd(3*6), &
      pbc(3*1), pad(1*6), pac(1*1), prim(1*3*1*6), scale
      DO md = 1,6
        DO mc = 1,1
          DO mb = 1,3
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_1_6
  SUBROUTINE block_1_3_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*md_max), kbc(3*1), kad(1*md_max), kac(1*1), &
      pbd(3*md_max), pbc(3*1), pad(1*md_max), pac(1*1), prim(1*3*1*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,1
          DO mb = 1,3
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_1
  SUBROUTINE block_1_3_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*1), kbc(3*2), kad(1*1), kac(1*2), pbd(3*1), &
      pbc(3*2), pad(1*1), pac(1*2), prim(1*3*2*1), scale
      DO md = 1,1
        DO mc = 1,2
          DO mb = 1,3
            kbc((mc-1)*3+mb) = kbc((mc-1)*3+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_2_1
  SUBROUTINE block_1_3_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*2), kbc(3*2), kad(1*2), kac(1*2), pbd(3*2), &
      pbc(3*2), pad(1*2), pac(1*2), prim(1*3*2*2), scale
      DO md = 1,2
        DO mc = 1,2
          DO mb = 1,3
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_2_2
  SUBROUTINE block_1_3_2_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*3), kbc(3*2), kad(1*3), kac(1*2), pbd(3*3), &
      pbc(3*2), pad(1*3), pac(1*2), prim(1*3*2*3), scale
      kbc(1:3*2) = 0.0_dp
      DO md = 1,3
        DO mc = 1,2
          DO mb = 1,3
            kbc((mc-1)*3+mb) = kbc((mc-1)*3+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_2_3
  SUBROUTINE block_1_3_3_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*1), kbc(3*3), kad(1*1), kac(1*3), pbd(3*1), &
      pbc(3*3), pad(1*1), pac(1*3), prim(1*3*3*1), scale
      DO md = 1,1
        DO mc = 1,3
          DO mb = 1,3
            kbd((md-1)*3+mb) = kbd((md-1)*3+mb) - ks_bd
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_3_1
  SUBROUTINE block_1_3_3_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*2), kbc(3*3), kad(1*2), kac(1*3), pbd(3*2), &
      pbc(3*3), pad(1*2), pac(1*3), prim(1*3*3*2), scale
      DO md = 1,2
        DO mc = 1,3
          DO mb = 1,3
            kbc((mc-1)*3+mb) = kbc((mc-1)*3+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_3_3_2
  SUBROUTINE block_1_3_5(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*md_max), kbc(3*5), kad(1*md_max), kac(1*5), &
      pbd(3*md_max), pbc(3*5), pad(1*md_max), pac(1*5), prim(1*3*5*md_max), &
      scale
      kbd(1:3*md_max) = 0.0_dp
      DO md = 1,md_max
      END DO
  END SUBROUTINE block_1_3_5
  SUBROUTINE block_1_3_6(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
      DO md = 1,md_max
      END DO
  END SUBROUTINE block_1_3_6
  SUBROUTINE block_1_4_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*1), kbc(4*1), kad(1*1), kac(1*1), pbd(4*1), &
      pbc(4*1), pad(1*1), pac(1*1), prim(1*4*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,4
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_1_1
  SUBROUTINE block_1_4_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*2), kbc(4*1), kad(1*2), kac(1*1), pbd(4*2), &
      pbc(4*1), pad(1*2), pac(1*1), prim(1*4*1*2), scale
      DO md = 1,2
        DO mc = 1,1
          DO mb = 1,4
            kbc((mc-1)*4+mb) = kbc((mc-1)*4+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_1_2
  SUBROUTINE block_1_4_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*3), kbc(4*1), kad(1*3), kac(1*1), pbd(4*3), &
      pbc(4*1), pad(1*3), pac(1*1), prim(1*4*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,4
            kbc((mc-1)*4+mb) = kbc((mc-1)*4+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_1_3
  SUBROUTINE block_1_4_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*1), kbc(4*2), kad(1*1), kac(1*2), pbd(4*1), &
      pbc(4*2), pad(1*1), pac(1*2), prim(1*4*2*1), scale
      DO md = 1,1
        DO mc = 1,2
          DO mb = 1,4
            kbc((mc-1)*4+mb) = kbc((mc-1)*4+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_2_1
  SUBROUTINE block_1_4_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*2), kbc(4*2), kad(1*2), kac(1*2), pbd(4*2), &
      pbc(4*2), pad(1*2), pac(1*2), prim(1*4*2*2), scale
      DO md = 1,2
        DO mc = 1,2
          DO mb = 1,4
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_2_2
  SUBROUTINE block_1_4_3_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*1), kbc(4*3), kad(1*1), kac(1*3), pbd(4*1), &
      pbc(4*3), pad(1*1), pac(1*3), prim(1*4*3*1), scale
      DO md = 1,1
        DO mc = 1,3
          DO mb = 1,4
            kbc((mc-1)*4+mb) = kbc((mc-1)*4+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_3_1
  SUBROUTINE block_1_4_3(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*md_max), kbc(4*3), kad(1*md_max), kac(1*3), &
      pbd(4*md_max), pbc(4*3), pad(1*md_max), pac(1*3), prim(1*4*3*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,3
          DO mb = 1,4
            kbc((mc-1)*4+mb) = kbc((mc-1)*4+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_3
  SUBROUTINE block_1_4_4_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*1), kbc(4*4), kad(1*1), kac(1*4), pbd(4*1), &
      pbc(4*4), pad(1*1), pac(1*4), prim(1*4*4*1), scale
      DO md = 1,1
        DO mc = 1,4
          DO mb = 1,4
            kbc((mc-1)*4+mb) = kbc((mc-1)*4+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_4_1
  SUBROUTINE block_1_4_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*md_max), kbc(4*4), kad(1*md_max), kac(1*4), &
      pbd(4*md_max), pbc(4*4), pad(1*md_max), pac(1*4), prim(1*4*4*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,4
          DO mb = 1,4
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_4_4
  SUBROUTINE block_1_5_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(5*3), kbc(5*1), kad(1*3), kac(1*1), pbd(5*3), &
      pbc(5*1), pad(1*3), pac(1*1), prim(1*5*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,5
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_5_1_3
  SUBROUTINE block_1_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(5*md_max), kbc(5*1), kad(1*md_max), kac(1*1), &
      pbd(5*md_max), pbc(5*1), pad(1*md_max), pac(1*1), prim(1*5*1*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,1
          DO mb = 1,5
            kbc((mc-1)*5+mb) = kbc((mc-1)*5+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_5_1
  SUBROUTINE block_1_6_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(6*1), kbc(6*1), kad(1*1), kac(1*1), pbd(6*1), &
      pbc(6*1), pad(1*1), pac(1*1), prim(1*6*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,6
            DO ma = 1,1
              kac((mc-1)*1+ma) = kac((mc-1)*1+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_6_1_1
  SUBROUTINE block_1_6_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(6*2), kbc(6*1), kad(1*2), kac(1*1), pbd(6*2), &
      pbc(6*1), pad(1*2), pac(1*1), prim(1*6*1*2), scale
      DO md = 1,2
        DO mc = 1,1
          DO mb = 1,6
            DO ma = 1,1
              kad((md-1)*1+ma) =  kad((md-1)*1+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_6_1_2
  SUBROUTINE block_1_6_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(6*3), kbc(6*1), kad(1*3), kac(1*1), pbd(6*3), &
      pbc(6*1), pad(1*3), pac(1*1), prim(1*6*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,6
            kbc((mc-1)*6+mb) = kbc((mc-1)*6+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_6_1_3
  SUBROUTINE block_1_6_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(6*1), kbc(6*2), kad(1*1), kac(1*2), pbd(6*1), &
      pbc(6*2), pad(1*1), pac(1*2), prim(1*6*2*1), scale
      DO md = 1,1
        DO mc = 1,2
          DO mb = 1,6
            kbc((mc-1)*6+mb) = kbc((mc-1)*6+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_1_6_2_1
  SUBROUTINE block_2_1_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*3), kbc(1*1), kad(2*3), kac(2*1), pbd(1*3), &
      pbc(1*1), pad(2*3), pac(2*1), prim(2*1*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,1
            DO ma = 1,2
              kac((mc-1)*2+ma) = kac((mc-1)*2+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_1_3
  SUBROUTINE block_2_1_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*4), kbc(1*1), kad(2*4), kac(2*1), pbd(1*4), &
      pbc(1*1), pad(2*4), pac(2*1), prim(2*1*1*4), scale
      DO md = 1,4
        DO mc = 1,1
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_1_4
  SUBROUTINE block_2_1_1_5(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*5), kbc(1*1), kad(2*5), kac(2*1), pbd(1*5), &
      pbc(1*1), pad(2*5), pac(2*1), prim(2*1*1*5), scale
      DO md = 1,5
        DO mc = 1,1
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_1_5
  SUBROUTINE block_2_1_1_6(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*6), kbc(1*1), kad(2*6), kac(2*1), pbd(1*6), &
      pbc(1*1), pad(2*6), pac(2*1), prim(2*1*1*6), scale
      DO md = 1,6
        DO mc = 1,1
          DO mb = 1,1
            DO ma = 1,2
              kad((md-1)*2+ma) =  kad((md-1)*2+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_1_6
  SUBROUTINE block_2_1_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*1), kbc(1*2), kad(2*1), kac(2*2), pbd(1*1), &
      pbc(1*2), pad(2*1), pac(2*2), prim(2*1*2*1), scale
      DO md = 1,1
        DO mc = 1,2
          DO mb = 1,1
            DO ma = 1,2
              kac((mc-1)*2+ma) = kac((mc-1)*2+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_2_1
  SUBROUTINE block_2_1_2_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*2), kbc(1*2), kad(2*2), kac(2*2), pbd(1*2), &
      pbc(1*2), pad(2*2), pac(2*2), prim(2*1*2*2), scale
      DO md = 1,2
        DO mc = 1,2
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_2_2
  SUBROUTINE block_2_1_2_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*4), kbc(1*2), kad(2*4), kac(2*2), pbd(1*4), &
      pbc(1*2), pad(2*4), pac(2*2), prim(2*1*2*4), scale
      DO md = 1,4
        DO mc = 1,2
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_1_2_4
  SUBROUTINE block_2_2_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*1), kbc(2*1), kad(2*1), kac(2*1), pbd(2*1), &
      pbc(2*1), pad(2*1), pac(2*1), prim(2*2*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_2_1_1
  SUBROUTINE block_2_2_2_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*1), kbc(2*2), kad(2*1), kac(2*2), pbd(2*1), &
      pbc(2*2), pad(2*1), pac(2*2), prim(2*2*2*1), scale
      DO md = 1,1
        DO mc = 1,2
          DO mb = 1,2
            kbd((md-1)*2+mb) = kbd((md-1)*2+mb) - ks_bd
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_2_2_1
  SUBROUTINE block_2_2_3_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*1), kbc(2*3), kad(2*1), kac(2*3), pbd(2*1), &
      pbc(2*3), pad(2*1), pac(2*3), prim(2*2*3*1), scale
      DO md = 1,1
        DO mc = 1,3
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_2_2_3_1
  SUBROUTINE block_3_2_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*md_max), kbc(2*1), kad(3*md_max), kac(3*1), &
      pbd(2*md_max), pbc(2*1), pad(3*md_max), pac(3*1), prim(3*2*1*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,1
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_3_2_1
  SUBROUTINE block_3_5_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(5*1), kbc(5*1), kad(3*1), kac(3*1), pbd(5*1), &
      pbc(5*1), pad(3*1), pac(3*1), prim(3*5*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,5
            DO ma = 1,3
              kad((md-1)*3+ma) =  kad((md-1)*3+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_3_5_1_1
  SUBROUTINE block_3_5_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(5*md_max), kbc(5*1), kad(3*md_max), kac(3*1), &
      pbd(5*md_max), pbc(5*1), pad(3*md_max), pac(3*1), prim(3*5*1*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,1
          DO mb = 1,5
            kbc((mc-1)*5+mb) = kbc((mc-1)*5+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_3_5_1
  SUBROUTINE block_3_6(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(6*md_max), kbc(6*mc_max), kad(3*md_max), &
      kac(3*mc_max), pbd(6*md_max), pbc(6*mc_max), pad(3*md_max), &
      pac(3*mc_max), prim(3*6*mc_max*md_max), scale
      kbd(1:6*md_max) = 0.0_dp
  END SUBROUTINE block_3_6
  SUBROUTINE block_3_9(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(9*md_max), kbc(9*mc_max), kad(3*md_max), &
      kac(3*mc_max), pbd(9*md_max), pbc(9*mc_max), pad(3*md_max), &
      pac(3*mc_max), prim(3*9*mc_max*md_max), scale
      DO md = 1,md_max
        DO mc = 1,mc_max
          DO mb = 1,9
            DO ma = 1,3
              kac((mc-1)*3+ma) = kac((mc-1)*3+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_3_9
  SUBROUTINE block_4_1_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*2), kbc(1*1), kad(4*2), kac(4*1), pbd(1*2), &
      pbc(1*1), pad(4*2), pac(4*1), prim(4*1*1*2), scale
      DO md = 1,2
        DO mc = 1,1
          DO mb = 1,1
            DO ma = 1,4
              kac((mc-1)*4+ma) = kac((mc-1)*4+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_1_1_2
  SUBROUTINE block_4_1_1_3(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*3), kbc(1*1), kad(4*3), kac(4*1), pbd(1*3), &
      pbc(1*1), pad(4*3), pac(4*1), prim(4*1*1*3), scale
      DO md = 1,3
        DO mc = 1,1
          DO mb = 1,1
            kbd((md-1)*1+mb) = kbd((md-1)*1+mb) - ks_bd
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_1_1_3
  SUBROUTINE block_4_1_1_4(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*4), kbc(1*1), kad(4*4), kac(4*1), pbd(1*4), &
      pbc(1*1), pad(4*4), pac(4*1), prim(4*1*1*4), scale
      DO md = 1,4
        DO mc = 1,1
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_1_1_4
  SUBROUTINE block_4_1_1(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*md_max), kbc(1*1), kad(4*md_max), kac(4*1), &
      pbd(1*md_max), pbc(1*1), pad(4*md_max), pac(4*1), prim(4*1*1*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,1
          DO mb = 1,1
            kbc((mc-1)*1+mb) = kbc((mc-1)*1+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_1_1
  SUBROUTINE block_4_1_4(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(1*md_max), kbc(1*4), kad(4*md_max), kac(4*4), &
      pbd(1*md_max), pbc(1*4), pad(4*md_max), pac(4*4), prim(4*1*4*md_max), &
      scale
      kbd(1:1*md_max) = 0.0_dp
  END SUBROUTINE block_4_1_4
  SUBROUTINE block_4_2_1_2(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*2), kbc(2*1), kad(4*2), kac(4*1), pbd(2*2), &
      pbc(2*1), pad(4*2), pac(4*1), prim(4*2*1*2), scale
      DO md = 1,2
        DO mc = 1,1
          DO mb = 1,2
            DO ma = 1,4
              kac((mc-1)*4+ma) = kac((mc-1)*4+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_2_1_2
  SUBROUTINE block_4_2_2(md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(2*md_max), kbc(2*2), kad(4*md_max), kac(4*2), &
      pbd(2*md_max), pbc(2*2), pad(4*md_max), pac(4*2), prim(4*2*2*md_max), &
      scale
      DO md = 1,md_max
        DO mc = 1,2
          DO mb = 1,2
            kbc((mc-1)*2+mb) = kbc((mc-1)*2+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_2_2
  SUBROUTINE block_4_3_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*1), kbc(3*1), kad(4*1), kac(4*1), pbd(3*1), &
      pbc(3*1), pad(4*1), pac(4*1), prim(4*3*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,3
            DO ma = 1,4
              kac((mc-1)*4+ma) = kac((mc-1)*4+ma)-tmp*p_bd
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_3_1_1
  SUBROUTINE block_4_3(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(3*md_max), kbc(3*mc_max), kad(4*md_max), &
      kac(4*mc_max), pbd(3*md_max), pbc(3*mc_max), pad(4*md_max), &
      pac(4*mc_max), prim(4*3*mc_max*md_max), scale
      DO md = 1,md_max
        DO mc = 1,mc_max
          DO mb = 1,3
            kbc((mc-1)*3+mb) = kbc((mc-1)*3+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_3
  SUBROUTINE block_4_4_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(4*1), kbc(4*1), kad(4*1), kac(4*1), pbd(4*1), &
      pbc(4*1), pad(4*1), pac(4*1), prim(4*4*1*1), scale
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,4
            DO ma = 1,4
              kad((md-1)*4+ma) =  kad((md-1)*4+ma)-tmp*p_bc
            END DO
          END DO
        END DO
      END DO
  END SUBROUTINE block_4_4_1_1
  SUBROUTINE block_15_15(mc_max,md_max,kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    REAL(KIND=dp) :: kbd(15*md_max), kbc(15*mc_max), kad(15*md_max), &
      kac(15*mc_max), pbd(15*md_max), pbc(15*mc_max), pad(15*md_max), &
      pac(15*mc_max), prim(15*15*mc_max*md_max), scale
      DO md = 1,md_max
        DO mc = 1,mc_max
          DO mb = 1,15
            kbc((mc-1)*15+mb) = kbc((mc-1)*15+mb) - ks_bc
          END DO
        END DO
      END DO
  END SUBROUTINE block_15_15
END MODULE hfx_contract_block
