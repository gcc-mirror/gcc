*
*  Originally derived from LAPACK 3.0 test suite failure.
*
*  David Billinghurst, (David.Billinghurst@riotinto.com.au)
*  23 February 2000
* 
      INTEGER N, I, SLASQX
      N = 20
      I = SLASQX( N ) 
      IF ( I .NE. 2*N ) THEN
         WRITE(6,*) 'I = ', I, ' but should be ', 2*N
         CALL ABORT()
      END IF
      END

      INTEGER FUNCTION SLASQX( N )
      INTEGER  N, I0, I, K
      I0 = 1
      DO I = 4*I0, 2*( I0+N-1 ), 4
         K = I
      END DO
      SLASQX = K
      RETURN
      END
