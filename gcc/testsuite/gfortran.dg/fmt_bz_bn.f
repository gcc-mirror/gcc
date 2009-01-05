c     { dg-do run }
c     PR38097 I/O with blanks in exponent fails; BN edit descriptor
c     Test case derived from reporter.
      character(11)  :: a = ' 2.  3 e+ 3'
      character(11)  :: b = ' 2.003 e+ 3'
      character(11)  :: c = ' 2.002 e+1 '
      real :: f

      f = 0.0
      read (a,'(BZ,E11.0)') f
      if (f .ne. 2003.0) call abort
      f = 0.0
      read (a,'(BN,E11.0)') f
      if (f .ne. 2300.0) call abort
      f = 0.0
      read (b,'(BN,E11.0)') f
      if (f .ne. 2003.0) call abort
      f = 0.0
      read (c,'(E11.0)') f
      if (f .ne. 20.020) call abort
      f = 0.0
      read (c,'(BZ,E11.0)') f
      if (f .ne. 2.002e10) call abort

      end
c     end of program

