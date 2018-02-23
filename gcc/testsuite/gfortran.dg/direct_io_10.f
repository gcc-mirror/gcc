! { dg-do run }
! pr35699 run-time abort writing zero sized section to direct access file
      program directio
      call       qi0010 (  10,   1,   2,   3,   4,  9,   2)
      end

      subroutine qi0010 (nf10, nf1, nf2, nf3, nf4,nf9, np2)
      character(10) bda(nf10)
      character(10) bda1(nf10), bval

      integer  j_len
      bda1(1) = 'x'
      do i = 2,10
        bda1(i) = 'x'//bda1(i-1)
      enddo
      bda = 'unread'

      inquire(iolength = j_len) bda1(nf1:nf10:nf2), bda1(nf4:nf3),
     $                               bda1(nf2:nf10:nf2)

      open (unit=48,
     $      access='direct',
     $      status='scratch',
     $      recl = j_len,
     $      iostat = istat,
     $      form='unformatted',
     $      action='readwrite')

      write (48,iostat = istat, rec = 3) bda1(nf1:nf10:nf2),
     $                    bda1(nf4:nf3), bda1(nf2:nf10:nf2)
      if ( istat .ne. 0) then
        STOP 1
      endif
      istat = -314

      read (48,iostat = istat, rec = np2+1) bda(nf1:nf9:nf2),
     $                       bda(nf4:nf3), bda(nf2:nf10:nf2)
      if ( istat .ne. 0) then
        STOP 2
      endif

      do j1 = 1,10
        bval = bda1(j1)
        if (bda(j1) .ne. bval) STOP 3
      enddo
      end subroutine
