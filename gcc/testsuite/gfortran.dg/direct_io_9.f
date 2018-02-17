! { dg-do run }
! PR34876 can't read/write zero length array sections
! Test case from PR by Dick Hendrikson
      program qi0011
      character(9) bda(10)
      character(9) bda1(10)
      integer  j_len
      istat = -314

      inquire(iolength = j_len) bda1

      istat = -314
      open (unit=48,
     $      status='scratch',
     $      access='direct',
     $      recl = j_len,
     $      iostat = istat,
     $      form='unformatted',
     $      action='readwrite')


      if (istat /= 0) STOP 1

      bda  = 'xxxxxxxxx'
      bda1 = 'yyyyyyyyy'
      write (48,iostat = istat, rec = 10) bda1(4:3)
      if ( istat .ne. 0) then
        STOP 2
      endif

      istat = -314
      read (48,iostat = istat, rec=10) bda(4:3)
      if ( istat .ne. 0) then
        STOP 3
      endif
      if (any(bda1.ne.'yyyyyyyyy')) STOP 4
      if (any(bda.ne.'xxxxxxxxx')) STOP 5
      end

