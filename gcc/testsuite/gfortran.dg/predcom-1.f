! PR 32160, complex temporary variables were not marked as gimple registers
! { dg-do compile }
! { dg-options "-O3" }

      REAL             FUNCTION CLANHT( N, E )
      INTEGER            N
      COMPLEX            E( * )
      INTEGER            I
      REAL               ANORM
      INTRINSIC          ABS
            DO 20 I = 2, N
               ANORM = ANORM +ABS( E( I ) )+ ABS( E( I-1 ) )
   20       CONTINUE
      CLANHT = ANORM
      RETURN
      END
