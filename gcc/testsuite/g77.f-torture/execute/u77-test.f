***   Some random stuff for testing libU77.  Should be done better.  It's
*     hard to test things where you can't guarantee the result.  Have a
*     good squint at what it prints, though detected errors will cause 
*     starred messages.
*
* Currently not tested:
*   ALARM
*   CHDIR (func)
*   CHMOD (func)
*   FGET (func/subr)
*   FGETC (func)
*   FPUT (func/subr)
*   FPUTC (func)
*   FSTAT (subr)
*   GETCWD (subr)
*   HOSTNM (subr)
*   IRAND
*   KILL
*   LINK (func)
*   LSTAT (subr)
*   RENAME (func/subr)
*   SIGNAL (subr)
*   SRAND
*   STAT (subr)
*   SYMLNK (func/subr)
*   UMASK (func)
*   UNLINK (func)
*
* NOTE! This is the testsuite version, so it should compile and
* execute on all targets, and either run to completion (with
* success status) or fail (by calling abort).  The *other* version,
* which is a bit more interactive and tests a couple of things
* this one cannot, should be generally the same, and is in
* libf2c/libU77/u77-test.f.  Please keep it up-to-date.

      implicit none

      external hostnm
*     intrinsic hostnm
      integer hostnm

      integer i, j, k, ltarray (9), idat (3), count, rate, count_max,
     +     pid, mask
      real tarray1(2), tarray2(2), r1, r2
      double precision d1
      integer(kind=2) bigi
      logical issum
      intrinsic getpid, getuid, getgid, ierrno, gerror, time8,
     +     fnum, isatty, getarg, access, unlink, fstat, iargc,
     +     stat, lstat, getcwd, gmtime, etime, chmod, itime, date,
     +     chdir, fgetc, fputc, system_clock, second, idate, secnds,
     +     time, ctime, fdate, ttynam, date_and_time, mclock, mclock8,
     +     cpu_time, dtime, ftell, abort
      external lenstr, ctrlc
      integer lenstr
      logical l
      character gerr*80, c*1
      character ctim*25, line*80, lognam*20, wd*100, line2*80, ddate*8,
     +     ttime*10, zone*5, ctim2*25
      integer fstatb (13), statb (13)
      integer *2 i2zero
      integer values(8)
      integer(kind=7) sigret

      i = time ()
      ctim = ctime (i)
      WRITE (6,'(A/)') '1 GNU libU77 test at: ' // ctim(:lenstr (ctim))
      write (6,'(A,I3,'', '',I3)')
     +     ' Logical units 5 and 6 correspond (FNUM) to'
     +     // ' Unix i/o units ', fnum(5), fnum(6)
      if (lnblnk('foo ').ne.3 .or. len_trim('foo ').ne.3) then
        print *, 'LNBLNK or LEN_TRIM failed'
        call abort
      end if

      bigi = time8 ()

      call ctime (i, ctim2)
      if (ctim .ne. ctim2) then
        write (6, *) '*** CALL CTIME disagrees with CTIME(): ',
     +    ctim2(:lenstr (ctim2)), ' vs. ', ctim(:lenstr (ctim))
        call doabort
      end if

      j = time ()
      if (i .gt. bigi .or. bigi .gt. j) then
        write (6, *) '*** TIME/TIME8/TIME sequence failures: ',
     +    i, bigi, j
        call doabort
      end if

      print *, 'Command-line arguments: ', iargc ()
      do i = 0, iargc ()
         call getarg (i, line)
         print *, 'Arg ', i, ' is: ', line(:lenstr (line))
      end do

      l= isatty(6)
      line2 = ttynam(6)
      if (l) then
        line = 'and 6 is a tty device (ISATTY) named '//line2
      else
        line = 'and 6 isn''t a tty device (ISATTY)'
      end if
      write (6,'(1X,A)') line(:lenstr(line))
      call ttynam (6, line)
      if (line .ne. line2) then
        print *, '*** CALL TTYNAM disagrees with TTYNAM: ',
     +    line(:lenstr (line))
        call doabort
      end if

*     regression test for compiler crash fixed by JCB 1998-08-04 com.c
      sigret = signal(2, ctrlc)

      pid = getpid()
      WRITE (6,'(A,I10)') ' Process id (GETPID): ', pid
      WRITE (6,'(A,I10)') ' User id (GETUID): ', GETUID ()
      WRITE (6,'(A,I10)') ' Group id (GETGID): ', GETGID ()
      WRITE (6, *) 'If you have the `id'' program, the following call'
      write (6, *) 'of SYSTEM should agree with the above:'
      call flush(6)
      CALL SYSTEM ('echo " " `id`')
      call flush

      lognam = 'blahblahblah'
      call getlog (lognam)
      write (6,*) 'Login name (GETLOG): ', lognam(:lenstr (lognam))

      wd = 'blahblahblah'
      call getenv ('LOGNAME', wd)
      write (6,*) 'Login name (GETENV of LOGNAME): ', wd(:lenstr (wd))

      call umask(0, mask)
      write(6,*) 'UMASK returns', mask
      call umask(mask)

      ctim = fdate()
      write (6,*) 'FDATE returns: ', ctim(:lenstr (ctim))
      call fdate (ctim)
      write (6,*) 'CALL FDATE returns: ', ctim(:lenstr (ctim))

      j=time()
      call ltime (j, ltarray)
      write (6,'(1x,a,9i4)') 'LTIME returns:', ltarray
      call gmtime (j, ltarray)
      write (6,'(1x,a,9i4)') 'GMTIME returns:', ltarray

      call system_clock(count)  ! omitting optional args
      call system_clock(count, rate, count_max)
      write(6,*) 'SYSTEM_CLOCK returns: ', count, rate, count_max

      call date_and_time(ddate)  ! omitting optional args
      call date_and_time(ddate, ttime, zone, values)
      write(6, *) 'DATE_AND_TIME returns: ', ddate, ' ', ttime, ' ',
     +     zone, ' ', values

      write (6,*) 'Sleeping for 1 second (SLEEP) ...'
      call sleep (1)

c consistency-check etime vs. dtime for first call
      r1 = etime (tarray1)
      r2 = dtime (tarray2)
      if (abs (r1-r2).gt.1.0) then
        write (6,*)
     +       'Results of ETIME and DTIME differ by more than a second:',
     +       r1, r2
        call doabort
      end if
      if (.not. issum (r1, tarray1(1), tarray1(2))) then
        write (6,*) '*** ETIME didn''t return sum of the array: ',
     +       r1, ' /= ', tarray1(1), '+', tarray1(2)
        call doabort
      end if
      if (.not. issum (r2, tarray2(1), tarray2(2))) then
        write (6,*) '*** DTIME didn''t return sum of the array: ',
     +       r2, ' /= ', tarray2(1), '+', tarray2(2)
        call doabort
      end if
      write (6, '(A,3F10.3)')
     +     ' Elapsed total, user, system time (ETIME): ',
     +     r1, tarray1

c now try to get times to change enough to see in etime/dtime
      write (6,*) 'Looping until clock ticks at least once...'
      do i = 1,1000
      do j = 1,1000
      end do
      call dtime (tarray2, r2)
      if (tarray2(1) .ne. 0. .or. tarray2(2) .ne. 0.) exit
      end do
      call etime (tarray1, r1)
      if (.not. issum (r1, tarray1(1), tarray1(2))) then
        write (6,*) '*** ETIME didn''t return sum of the array: ',
     +       r1, ' /= ', tarray1(1), '+', tarray1(2)
        call doabort
      end if
      if (.not. issum (r2, tarray2(1), tarray2(2))) then
        write (6,*) '*** DTIME didn''t return sum of the array: ',
     +       r2, ' /= ', tarray2(1), '+', tarray2(2)
        call doabort
      end if
      write (6, '(A,3F10.3)')
     +     ' Differences in total, user, system time (DTIME): ',
     +     r2, tarray2
      write (6, '(A,3F10.3)')
     +     ' Elapsed total, user, system time (ETIME): ',
     +     r1, tarray1
      write (6, *) '(Clock-tick detected after ', i, ' 1K loops.)'

      call idate (i,j,k)
      call idate (idat)
      write (6,*) 'IDATE (date,month,year): ',idat
      print *,  '... and the VXT version (month,date,year): ', i,j,k
      if (i/=idat(2) .or. j/=idat(1) .or. k/=mod(idat(3),100)) then
        print *, '*** VXT and U77 versions don''t agree'
        call doabort
      end if

      call date (ctim)
      write (6,*) 'DATE (dd-mmm-yy): ', ctim(:lenstr (ctim))

      call itime (idat)
      write (6,*) 'ITIME (hour,minutes,seconds): ', idat

      call time(line(:8))
      print *, 'TIME: ', line(:8)

      write (6,*) 'SECNDS(0.0) returns: ',secnds(0.0)

      write (6,*) 'SECOND returns: ', second()
      call dumdum(r1)
      call second(r1)
      write (6,*) 'CALL SECOND returns: ', r1

*     compiler crash fixed by 1998-10-01 com.c change
      if (rand(0).lt.0.0 .or. rand(0).gt.1.0) then
        write (6,*) '*** rand(0) error'
        call doabort()
      end if

      i = getcwd(wd)
      if (i.ne.0) then
        call perror ('*** getcwd')
        call doabort
      else
        write (6,*) 'Current directory is "'//wd(:lenstr(wd))//'"'
      end if
      call chdir ('.',i)
      if (i.ne.0) then
        write (6,*) '***CHDIR to ".": ', i
        call doabort
      end if

      i=hostnm(wd)
      if(i.ne.0) then
        call perror ('*** hostnm')
        call doabort
      else
        write (6,*) 'Host name is ', wd(:lenstr(wd))
      end if

      i = access('/dev/null ', 'rw')
      if (i.ne.0) write (6,*) '***Read/write ACCESS to /dev/null: ', i
      write (6,*) 'Creating file "foo" for testing...'
      open (3,file='foo',status='UNKNOWN')
      rewind 3
      call fputc(3, 'c',i)
      call fputc(3, 'd',j)      
      if (i+j.ne.0) write(6,*) '***FPUTC: ', i
C     why is it necessary to reopen?  (who wrote this?)
C     the better to test with, my dear!  (-- burley)
      close(3)
      open(3,file='foo',status='old')
      call fseek(3,0,0,*10)
      go to 20
 10   write(6,*) '***FSEEK failed'
      call doabort
 20   call fgetc(3, c,i)
      if (i.ne.0) then
        write(6,*) '***FGETC: ', i
        call doabort
      end if
      if (c.ne.'c') then
        write(6,*) '***FGETC read the wrong thing: ', ichar(c)
        call doabort
      end if
      i= ftell(3)
      if (i.ne.1) then
        write(6,*) '***FTELL offset: ', i
        call doabort
      end if
      call ftell(3, i)
      if (i.ne.1) then
        write(6,*) '***CALL FTELL offset: ', i
        call doabort
      end if
      call chmod ('foo', 'a+w',i)
      if (i.ne.0) then
        write (6,*) '***CHMOD of "foo": ', i
        call doabort
      end if
      i = fstat (3, fstatb)
      if (i.ne.0) then
        write (6,*) '***FSTAT of "foo": ', i
        call doabort
      end if
      i = stat ('foo', statb)
      if (i.ne.0) then
        write (6,*) '***STAT of "foo": ', i
        call doabort
      end if
      write (6,*) '  with stat array ', statb
      if (statb(6) .ne. getgid ()) then
        write (6,*) 'Note: FSTAT gid wrong (happens on some systems).'
      end if
      if (statb(5) .ne. getuid () .or. statb(4) .ne. 1) then
        write (6,*) '*** FSTAT uid or nlink is wrong'
        call doabort
      end if
      do i=1,13
        if (fstatb (i) .ne. statb (i)) then
          write (6,*) '*** FSTAT and STAT don''t agree on '// '
     +         array element ', i, ' value ', fstatb (i), statb (i)
          call abort
        end if
      end do
      i = lstat ('foo', fstatb)
      do i=1,13
        if (fstatb (i) .ne. statb (i)) then
          write (6,*) '*** LSTAT and STAT don''t agree on '//
     +         'array element ', i, ' value ', fstatb (i), statb (i)
          call abort
        end if
      end do

C     in case it exists already:
      call unlink ('bar',i)
      call link ('foo ', 'bar ',i)
      if (i.ne.0) then
        write (6,*) '***LINK "foo" to "bar" failed: ', i
        call doabort
      end if
      call unlink ('foo',i)
      if (i.ne.0) then
        write (6,*) '***UNLINK "foo" failed: ', i
        call doabort
      end if
      call unlink ('foo',i)
      if (i.eq.0) then
        write (6,*) '***UNLINK "foo" again: ', i
        call doabort
      end if

      call gerror (gerr)
      i = ierrno()
      write (6,'(A,I3,A/1X,A)') ' The current error number is: ',
     +     i,
     +     ' and the corresponding message is:', gerr(:lenstr(gerr))
      write (6,*) 'This is sent to stderr prefixed by the program name'
      call getarg (0, line)
      call perror (line (:lenstr (line)))
      call unlink ('bar')

      print *, 'MCLOCK returns ', mclock ()
      print *, 'MCLOCK8 returns ', mclock8 ()

      call cpu_time (d1)
      print *, 'CPU_TIME returns ', d1

C     WRITE (6,*) 'You should see exit status 1'
      CALL EXIT(0)
 99   END

* Return length of STR not including trailing blanks, but always > 0.
      integer function lenstr (str)
      character*(*) str
      if (str.eq.' ') then
        lenstr=1
      else
        lenstr = lnblnk (str)
      end if
      end

* Just make sure SECOND() doesn't "magically" work the second time.
      subroutine dumdum(r)
      r = 3.14159
      end

* Test whether sum is approximately left+right.
      logical function issum (sum, left, right)
      implicit none
      real sum, left, right
      real mysum, delta, width
      mysum = left + right
      delta = abs (mysum - sum)
      width = abs (left) + abs (right)
      issum = (delta .le. .0001 * width)
      end

* Signal handler
      subroutine ctrlc
      print *, 'Got ^C'
      call doabort
      end

* A problem has been noticed, so maybe abort the test.
      subroutine doabort
* For this version, call the ABORT intrinsic.
      intrinsic abort
      call abort
      end

* Testsuite version only.
* Don't actually reference the HOSTNM intrinsic, because some targets
* need -lsocket, which we don't have a mechanism for supplying.
      integer function hostnm(nm)
      character*(*) nm
      nm = 'not determined by this version of u77-test.f'
      hostnm = 0
      end
