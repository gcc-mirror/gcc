C Test profile-directed block ordering with various Fortran 77 constructs
C to catch basic regressions in the functionality.

      program bprob1
      implicit none
      integer i,j,k,n
      integer result
      integer lpall, ieall, gtall
      integer lpval, ieval, gtval

      lpval = lpall()
      ieval = ieall()
      gtval = gtall()
      if ((lpval .ne. 1) .or. (ieval .ne. 1) .or. (gtval .ne. 1)) then
         call abort
      end if
      
      end

C Pass a value through a function to thwart optimization.
      integer function foo(i)
      implicit none
      integer i
      foo = i
      end

C Test various flavors of GOTO and compare results against expected values.
      integer function gtall()
      implicit none
      integer gt1, gt2, gt3, gt4, gt5
      integer gtval

      gtall = 1
      gtval = 0
      gtval = gtval + gt1(0)
      gtval = gtval + gt1(1)
      if (gtval .ne. 3) then
         print *,"gtall part 1:  ", gtval, 3
         gtall = 0
      end if

      gtval = 0
      gtval = gtval + gt2(3)
      gtval = gtval + gt2(30)
      if (gtval .ne. 12) then
         print *,"gtall part 2:  ", gtval, 12
         gtall = 0
      end if

      gtval = 0
      gtval = gtval + gt3(0)
      gtval = gtval + gt3(3)
      if (gtval .ne. 48) then
         print *,"gtall part 3:  ", gtval, 48
         gtall = 0
      end if

      gtval = 0
      gtval = gtval + gt4(1)
      gtval = gtval + gt4(2)
      gtval = gtval + gt4(3)
      if (gtval .ne. 14) then
         print *,"gtall part 4:  ", gtval, 14
         gtall = 0
      end if

      gtval = 0
      gtval = gtval + gt5(0)
      gtval = gtval + gt5(-1)
      gtval = gtval + gt5(5)
      if (gtval .ne. 14) then
         print *,"gtall part 5:  ", gtval, 14
         gtall = 0
      end if
      end

C Test simple GOTO.
      integer function gt1(f)
      implicit none
      integer f
      if (f .ne. 0) goto 100
      gt1 = 1
      goto 101
  100 gt1 = 2
  101 continue
      end

C Test simple GOTO again, this time out of a DO loop.
      integer function gt2(f)
      implicit none
      integer f
      integer i
      do i=1,10
         if (i .eq. f) goto 100
      end do
      gt2 = 4
      goto 101
  100 gt2 = 8
  101 continue
      end

C Test computed GOTO.
      integer function gt3(i)
      implicit none
      integer i
      gt3 = 8
      goto (101, 102, 103, 104), i
      goto 105
  101 gt3 = 1024
      goto 105
  102 gt3 = 2048
      goto 105
  103 gt3 = 16
      goto 105
  104 gt3 = 4096
      goto 105
  105 gt3 = gt3 * 2
      end

C Test assigned GOTO.
      integer function gt4(i)
      implicit none
      integer i
      integer label
      assign 101 to label
      if (i .eq. 2) assign 102 to label
      if (i .eq. 3) assign 103 to label
      goto label, (101, 102, 103)
  101 gt4 = 1
      goto 104
  102 gt4 = 2
      goto 104
  103 gt4 = 4
  104 gt4 = gt4 * 2
      end

C Test arithmetic IF (bundled with the GOTO variants).
      integer function gt5(i)
      implicit none
      integer i
      gt5 = 1
      if (i) 101, 102, 103
  101 gt5 = 2
      goto 104
  102 gt5 = 4
      goto 104
  103 gt5 = 8
  104 continue
      end

C Run all of the loop tests and check results against expected values.
      integer function lpall()
      implicit none
      integer loop1, loop2
      integer loopval

      lpall = 1
      loopval = 0
      loopval = loopval + loop1(1,0)
      loopval = loopval + loop1(1,2)
      loopval = loopval + loop1(1,7)
      if (loopval .ne. 12) then
         print *,"lpall part 1:  ", loopval, 12
         lpall = 0
      end if

      loopval = 0
      loopval = loopval + loop2(1,0,0,0)
      loopval = loopval + loop2(1,1,0,0)
      loopval = loopval + loop2(1,1,3,0)
      loopval = loopval + loop2(1,1,3,1)
      loopval = loopval + loop2(1,3,1,5)
      loopval = loopval + loop2(1,3,7,3)
      if (loopval .ne. 87) then
         print *,"lpall part 2:  ", loopval, 87
         lpall = 0
      end if
      end

C Test a simple DO loop.
      integer function loop1(r,n)
      implicit none
      integer r,n,i

      loop1 = r
      do i=1,n
         loop1 = loop1 + 1
      end do
      end

C Test nested DO loops.
      integer function loop2(r, l, m, n)
      implicit none
      integer r,l,m,n
      integer i,j,k
      loop2 = r
      do i=1,l
         do j=1,m
            do k=1,n
               loop2 = loop2 + 1
            end do
         end do
      end do
      end

C Test various combinations of IF-THEN-ELSE and check results against
C expected values.
      integer function ieall()
      implicit none
      integer ie1, ie2, ie3
      integer ieval
      ieall = 1
      ieval = 0
      
      ieval = ieval + ie1(0,2)
      ieval = ieval + ie1(0,0)
      ieval = ieval + ie1(1,2)
      ieval = ieval + ie1(10,2)
      ieval = ieval + ie1(11,11)
      if (ieval .ne. 31) then
         print *,"ieall part 1:  ", ieval, 31
         ieall = 0
      end if

      ieval = 0
      ieval = ieval + ie2(0)
      ieval = ieval + ie2(2)
      ieval = ieval + ie2(2)
      ieval = ieval + ie2(2)
      ieval = ieval + ie2(3)
      ieval = ieval + ie2(3)
      if (ieval .ne. 23) then
         print *,"ieall part 2:  ", ieval, 23
         ieall = 0
      end if

      ieval = 0
      ieval = ieval + ie3(11,19)
      ieval = ieval + ie3(25,27)
      ieval = ieval + ie3(11,22)
      ieval = ieval + ie3(11,10)
      ieval = ieval + ie3(21,32)
      ieval = ieval + ie3(21,20)
      ieval = ieval + ie3(1,2)
      ieval = ieval + ie3(32,31)
      ieval = ieval + ie3(3,0)
      ieval = ieval + ie3(0,47)
      ieval = ieval + ie3(65,65)
      if (ieval .ne. 246) then
         print *,"ieall part 3:  ", ieval, 246
         ieall = 0
      end if
      end

C Test IF-THEN-ELSE.
      integer function ie1(i,j)
      implicit none
      integer i,j
      integer foo

      ie1 = 0
      if (i .ne. 0) then
         if (j .ne. 0) then
            ie1 = foo(4)
         else
            ie1 = foo(1024)
         end if
      else
         if (j .ne. 0) then
            ie1 = foo(1)
         else
            ie1 = foo(2)
         end if
      end if
      if (i .gt. j) then
         ie1 = foo(ie1*2)
      end if
      if (i .gt. 10) then
         if (j .gt. 10) then
            ie1 = foo(ie1*4)
         end if
      end if
      end

C Test a series of simple IF-THEN statements.
      integer function ie2(i)
      implicit none
      integer i
      integer foo
      ie2 = 0

      if (i .eq. 0) then
         ie2 = foo(1)
      end if
      if (i .eq. 1) then
         ie2 = foo(1024)
      end if
      if (i .eq. 2) then
         ie2 = foo(2)
      end if
      if (i .eq. 3) then
         ie2 = foo(8)
      end if
      if (i .eq. 4) then
         ie2 = foo(2048)
      end if

      end

C Test nested IF statements and IF with compound expressions.
      integer function ie3(i,j)
      implicit none
      integer i,j
      integer foo

      ie3 = 1
      if ((i .gt. 10) .and. (j .gt. i) .and. (j .lt. 20)) then
         ie3 = foo(16)
      end if
      if (i .gt. 20) then
         if (j .gt. i) then
            if (j .lt. 30) then
               ie3 = foo(32)
            end if
         end if
      end if
      if ((i .eq. 3) .or. (j .eq. 47) .or. (i .eq.j)) then
         ie3 = foo(64)
      end if
      end
