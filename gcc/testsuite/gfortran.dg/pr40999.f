! { dg-do compile }
! { dg-options "-O3 -fwhole-file" }

      SUBROUTINE ZLARFG( ALPHA )
        ALPHA = ZLADIV( DCMPLX( 1.0D+0 ) )
      END
      COMPLEX*16 FUNCTION ZLADIV( X )
        COMPLEX*16         X
        CALL DLADIV( DBLE( X ), DIMAG( X ) )
      END

