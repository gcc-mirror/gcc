*
*  Derived from LAPACK 3.0 routine CHGEQZ
*  Fails on i686-pc-cygwin with gcc-2.97 snapshots at -O2 and higher
*  PR fortran/1645
*
*  David Billinghurst, (David.Billinghurst@riotinto.com)
*  14 January 2001
*  Rewritten by Toon Moene (toon@moene.indiv.nluug.nl)
*  15 January 2001
* 
      COMPLEX A(5,5)
      DATA A/25*(0.0,0.0)/
      A(4,3) = (0.05,0.2)/3.0E-7
      A(4,4) = (-0.03,-0.4)
      A(5,4) = (-2.0E-07,2.0E-07)
      CALL CHGEQZ( 5, A )
      END
      SUBROUTINE CHGEQZ( N, A )
      COMPLEX   A(N,N), X
      ABS1( X ) = ABS( REAL( X ) ) + ABS( AIMAG( X ) )
      DO J = 4, 2, -1
         I = J
         TEMP  = ABS1( A(J,J) )
         TEMP2 = ABS1( A( J+1, J ) )
         TEMPR = MAX( TEMP, TEMP2 )
         IF( TEMPR .LT. 1.0 .AND. TEMPR .NE. 0.0 ) THEN
            TEMP  = TEMP / TEMPR
            TEMP2 = TEMP2 / TEMPR
         END IF
         IF ( ABS1(A(J,J-1))*TEMP2 .LE. TEMP ) GO TO 90
      END DO
c     Should not reach here, but need a statement
      PRINT*
  90  IF ( I .NE. 4 ) THEN
         PRINT*,'I =', I, ' but should be 4'
         CALL ABORT()
      END IF
      END
