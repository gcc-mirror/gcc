c { dg-do compile }
      SUBROUTINE SGBTRF( M, KL, KU, AB, LDAB )

*  PR fortran/275
*  ICE in `change_address', at emit-rtl.c:1589 with -O1 and above
*  g77 version 2.96 20000530 (experimental) on mips-sgi-irix6.5/-mabi=64
*
*  Originally derived from LAPACK 3.0 test suite failure.
*
*  David Billinghurst, (David.Billinghurst@riotinto.com.au)
*  1 June 2000

      INTEGER   KL, KU, LDAB, M
      REAL      AB( LDAB, * )

      INTEGER   J, JB, JJ, JP, KV, KM
      REAL      WORK13(65,64), WORK31(65,64)
      KV = KU + KL
      DO J = 1, M
         JB = MIN( 1, M-J+1 )
         DO JJ = J, J + JB - 1
            KM = MIN( KL, M-JJ )
            JP = KM+1
            CALL SSWAP( JB, AB( KV+1+JJ-J, J ), LDAB-1,
     $           AB( KV+JP+JJ-J, J ), LDAB-1 )
         END DO
      END DO
      RETURN
      END
