      SUBROUTINE SORG2R( K, A, N, LDA )
*  ICE in `verify_wide_reg_1', at flow.c:2605 at -O2
*  g77 version 2.96 20000515 (experimental) on i686-pc-linux-gnu
*
*  Originally derived from LAPACK 3.0 test suite failure.
*
*  David Billinghurst, (David.Billinghurst@riotinto.com.au)
*  18 May 2000
      INTEGER            I, K, LDA, N
      REAL               A( LDA, * )
      DO I = K, 1, -1
         IF( I.LT.N ) A( I, I ) = 1.0
         A( I, I ) = 1.0
      END DO
      RETURN
      END
