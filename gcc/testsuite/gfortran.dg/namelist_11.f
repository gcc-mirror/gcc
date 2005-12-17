c { dg-do run }
c This program tests: namelist comment, a blank line before the nameilist name, the namelist name,
c a scalar qualifier, various combinations of space, comma and lf delimiters, f-formats, e-formats
c a blank line within the data read, nulls, a range qualifier, a new object name before end of data
c and an integer read.  It also tests that namelist output can be re-read by namelist input.
c provided by Paul Thomas - pault@gcc.gnu.org

      program namelist_1

      REAL x(10)
      REAL(kind=8) xx
      integer ier
      namelist /mynml/ x, xx

      do i = 1 , 10
        x(i) = -1
      end do
      x(6) = 6.0
      x(10) = 10.0
      xx = 0d0

      open (10,status="scratch")
      write (10, *) "!mynml"
      write (10, *) ""
      write (10, *) "&gf /"
      write (10, *) "&mynml  x(7) =+99.0e0 x=1.0, 2.0 ,"
      write (10, *) " 2*3.0, ,, 7.0e0,+0.08e+02 !comment"
      write (10, *) ""
      write (10, *) " 9000e-3 x(4:5)=4 ,5 "
      write (10, *) " x=,,3.0, xx=10d0 /"
      rewind (10)

      read (10, nml=mynml, IOSTAT=ier)
      if (ier.ne.0) call abort
      rewind (10)

      do i = 1 , 10
        if ( abs( x(i) - real(i) ) .gt. 1e-8 ) call abort
      end do
      if ( abs( xx - 10d0 ) .gt. 1e-8 ) call abort

      write (10, nml=mynml, iostat=ier)
      if (ier.ne.0) call abort
      rewind (10)

      read (10, NML=mynml, IOSTAT=ier)
      if (ier.ne.0) call abort
      close (10)

      do i = 1 , 10
        if ( abs( x(i) - real(i) ) .gt. 1e-8 ) call abort
      end do
      if ( abs( xx - 10d0 ) .gt. 1e-8 ) call abort

      end program
