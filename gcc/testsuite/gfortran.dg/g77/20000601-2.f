c { dg-do compile }
      SUBROUTINE SGBTRF( M, KL, KU, AB, LDAB )

*  Slightly modified version of 20000601-1.f that still ICES with
*  CVS 20010118 g77 on mips-sgi-irix6.5/-mabi=64.
*
*  Originally derived from LAPACK 3.0 test suite failure.
*
*  David Billinghurst, (David.Billinghurst@riotinto.com.au)
*  18 January 2001

      INTEGER   KL, KU, LDAB, M
      REAL      AB( LDAB, * )

      INTEGER   J, JB, JJ, JP, KV, KM, F
      REAL      WORK13(65,64), WORK31(65,64)
      KV = KU + KL
      DO J = 1, M
         JB = MIN( 1, M-J+1 )
         DO JJ = J, J + JB - 1
            KM = MIN( KL, M-JJ )
            JP = F( KM+1, AB( KV+1, JJ ) )
            CALL SSWAP( JB, AB( KV+1+JJ-J, J ), LDAB-1,
     $           AB( KV+JP+JJ-J, J ), LDAB-1 )
         END DO
      END DO
      RETURN
      END
