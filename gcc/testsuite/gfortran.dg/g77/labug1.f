c { dg-do run }
      PROGRAM LABUG1

*  This program core dumps on mips-sgi-irix6.2 when compiled
*  with egcs-19981101, egcs-19981109 and egcs-19981122 snapshots
*  with -O2
*
*  Originally derived from LAPACK test suite.
*  Almost any change allows it to run.
*
*  David Billinghurst, (David.Billinghurst@riotinto.com.au)
*  25 November 1998
* 
*     .. Parameters ..
      INTEGER   LDA, LDE
      PARAMETER ( LDA = 2500, LDE = 50  )
      COMPLEX   CZERO 
      PARAMETER ( CZERO = ( 0.0E+0, 0.0E+0 ) )

      INTEGER   I, J, M, N
      REAL      V
      COMPLEX   A(LDA),B(LDA),C(LDA),E(LDE,LDE),F(LDE,LDE)  
      COMPLEX   Z

      N=2
      M=1
*
      do i = 1, m
         do j = 1, n
            e(i,j) = czero
            f(i,j) = czero
        end do
      end do
*
      DO J = 1, N
         DO I = 1, M
            V =  ABS( E(I,J) - F(I,J) )
         END DO
      END DO
 
      CALL SUB2(M,Z)

      END

      subroutine SUB2(I,A)
      integer i
      complex a
      end










