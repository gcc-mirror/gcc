C Test Fortran 77 I edit descriptor for input
C      (ANSI X3.9-1978 Section 13.5.9.1)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C Scratch files aren't implemented for most simulators; usually a
C stub function returns an error code.
C { dg-do run { xfail mmix-knuth-mmixware cris-*-elf } }
      integer i,j

      open(unit=10,status='SCRATCH')
      write(10,'(A)') '1'
      write(10,'(A)') ' '
      write(10,'(A)') '   -1'

      rewind(10)

      read(10,'(I1)') i
      if ( i.ne.1 ) call abort()
      read(10,'(I1)') i
      if ( i.ne.0 ) call abort()
      read(10,'(I2,X,I2)') i,j
      if ( i.ne.0 ) call abort()
      if ( j.ne.-1 ) call abort()

      end
