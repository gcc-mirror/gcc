! { dg-do compile }
! { dg-options "-O2 -ffast-math -funroll-loops -ftree-vectorize -g" }
  SUBROUTINE block_15_1_1_1(kbd,kbc,kad,kac,pbd,pbc,pad,pac,prim,scale)
    INTEGER, PARAMETER :: dp=8
    REAL(KIND=dp) :: kbd(1*1), kbc(1*1), kad(15*1), kac(15*1), pbd(1*1), &
      pbc(1*1), pad(15*1), pac(15*1), prim(15*1*1*1), scale
    INTEGER                                  :: ma, mb, mc, md, p_index
      DO md = 1,1
        DO mc = 1,1
          DO mb = 1,1
            DO ma = 1,15
              p_index=p_index+1
              tmp = scale*prim(p_index)
              ks_bd = ks_bd + tmp* pac((mc-1)*15+ma)
            END DO
            kbd((md-1)*1+mb) = kbd((md-1)*1+mb) - ks_bd
          END DO
        END DO
      END DO
  END SUBROUTINE block_15_1_1_1
