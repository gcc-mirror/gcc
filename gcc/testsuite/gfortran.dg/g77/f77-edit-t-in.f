C Test Fortran 77 T edit descriptor for input
C      (ANSI X3.9-1978 Section 13.5.3.2)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-std=legacy" }
C
      integer i,j
      real a,b,c,d,e
      character*32 in

      in = '1234   8'
      read(in,'(T3,I1)') i
      if ( i.ne.3 )                   STOP 1
      read(in,'(5X,TL4,I2)') i
      if ( i.ne.23 )                  STOP 2
      read(in,'(3X,I1,TR3,I1)') i,j
      if ( i.ne.4 )                  STOP 3
      if ( j.ne.8 )                  STOP 4

      in = '   1.5  -12.62  348.75  1.0E-6'
 100  format(F6.0,TL6,I4,1X,I1,8X,I5,F3.0,T10,F5.0,T17,F6.0,TR2,F6.0)
      read(in,100) a,i,j,k,b,c,d,e
      if ( abs(a-1.5).gt.1.0e-5 )     STOP 5
      if ( i.ne.1 )                   STOP 6
      if ( j.ne.5 )                   STOP 7
      if ( k.ne.348 )                 STOP 8
      if ( abs(b-0.75).gt.1.0e-5 )    STOP 9
      if ( abs(c-12.62).gt.1.0e-5 )   STOP 10
      if ( abs(d-348.75).gt.1.0e-4 )  STOP 11
      if ( abs(e-1.0e-6).gt.1.0e-11 ) STOP 12
      end
