C Test compiler flags: -fno-f90
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C Read the g77 manual entry on CMPAMBIG
C
C { dg-do run }
C { dg-options "-fno-f90 -fugly-complex" }
      double complex z
      z = (2.0d0,1.0d0)
      call s(real(z))
      end
      subroutine s(x)
      real x
      if ( abs(x-2.0) .gt. 1.0e-5 ) call abort
      end
