! { dg-lto-do link }
SUBROUTINE mltfftsg ( a, ldax, lday )
  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND ( 14, 200 )
  INTEGER, INTENT ( IN ) :: ldax, lday
  COMPLEX ( dbl ), INTENT ( INOUT ) :: a ( ldax, lday )
END SUBROUTINE mltfftsg

