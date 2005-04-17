c{ dg-do run }
c This program repeats many of the same tests as test_nml_1 but for integer instead of real.
c  It also tests repeat nulls, comma delimited character read, a triplet qualifier, a range with
c and assumed start, a quote delimited string, a qualifier with an assumed end and a fully
c explicit range.  It also tests that integers and characters are successfully read back by
c namelist.
c Provided by Paul Thomas - pault@gcc.gnu.org

      program namelist_12

      integer*4 x(10)
      integer*8 xx
      integer ier
      character*10 ch , check
      namelist /mynml/ x, xx, ch
 
c set debug = 0 or 1 in the namelist! (line 33)

      do i = 1 , 10
        x(i) = -1
      end do
      x(6) = 6
      x(10) = 10
      xx = 0
      ch ="zzzzzzzzzz"
      check="abcdefghij"

      open (10,status="scratch")
      write (10, *) "!mynml"
      write (10, *) " "
      write (10, *) "&mynml  x(7) =+99 x=1, 2 ,"
      write (10, *) " 2*3, ,, 2* !comment"
      write (10, *) " 9 ch=qqqdefghqq , x(8:7:-1) = 8 , 7"
      write (10, *) " ch(:3) =""abc"","
      write (10, *) " ch(9:)='ij' x(4:5)=4 ,5 xx = 42/"
      rewind (10)

      read (10, nml=mynml, IOSTAT=ier)
      if (ier.ne.0) call abort
      rewind (10)

      write (10, nml=mynml, iostat=ier)
      if (ier.ne.0) call abort
      rewind (10)

      read (10, NML=mynml, IOSTAT=ier)
      if (ier.ne.0) call abort
      close (10)

      do i = 1 , 10
        if ( abs( x(i) - i ) .ne. 0 ) call abort ()
        if ( ch(i:i).ne.check(I:I) ) call abort
      end do
      if (xx.ne.42) call abort ()

      end program
