! { dg-options "-O2 -fgraphite-identity" }
# 1 "mltfftsg.F"
# 1 "<built-in>"
SUBROUTINE mltfftsg ( a, ldax, lday, b, ldbx, ldby, &
                      n, m)
  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )

! Arguments
  INTEGER, INTENT ( IN ) :: ldbx, ldby, n, m
  COMPLEX ( dbl ), INTENT ( INOUT ) :: b ( ldbx, ldby )

   B(N+1:LDBX,1:M) = CMPLX(0._dbl,0._dbl,dbl)
    
END SUBROUTINE mltfftsg
