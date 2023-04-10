! { dg-do compile }
! { dg-additional-options "-fnon-call-exceptions" }
! { dg-additional-options "-march=armv8.2-a+sve" { target aarch64*-*-* } }

MODULE hfx_contract_block
  INTEGER, PARAMETER :: dp=8
CONTAINS
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
END MODULE hfx_contract_block
