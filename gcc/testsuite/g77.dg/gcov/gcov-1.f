C { dg-options "-fprofile-arcs -ftest-coverage" }
C { dg-do run { target native } }
C
C Test gcov reports for line counts and branch and call return percentages
C for various Fortran 77 constructs to catch basic regressions in the
C functionality.

      program gcov1
      implicit none
      integer i,j,k,n
      integer result
      integer lpall, ieall, gtall
      integer lpval, ieval, gtval

					! returns(100)
      lpval = lpall()			! count(1)
					! returns(100)
      ieval = ieall()			! count(1)
					! returns(100)
      gtval = gtall()			! count(1)
					! returns(end)
      if ((lpval .ne. 1) .or. (ieval .ne. 1) .or. (gtval .ne. 1)) then
         call abort
      end if
      
      end

C Pass a value through a function to thwart optimization.
      integer function foo(i)
      implicit none
      integer i
      foo = i				! count(18)
      end

C Test various flavors of GOTO and compare results against expected values.
      integer function gtall()
      implicit none
      integer gt1, gt2, gt3, gt4, gt5
      integer gtval

      gtall = 1				! count(1)
      gtval = 0				! count(1)
					! returns(100)
      gtval = gtval + gt1(0)		! count(1)
					! returns(100)
      gtval = gtval + gt1(1)		! count(1)
					! returns(end)
					! branch(0)
      if (gtval .ne. 3) then		! count(1)
					! branch(end)
         print *,"gtall part 1:  ", gtval, 3
         gtall = 0
      end if

      gtval = 0				! count(1)
					! returns(100)
      gtval = gtval + gt2(9)		! count(1)
					! returns(100)
      gtval = gtval + gt2(20)		! count(1)
					! returns(end)
					! branch(0)
      if (gtval .ne. 12) then		! count(1)
					! branch(end)
         print *,"gtall part 2:  ", gtval, 12
         gtall = 0
      end if

      gtval = 0				! count(1)
					! returns(100)
      gtval = gtval + gt3(0)		! count(1)
					! returns(100)
      gtval = gtval + gt3(3)		! count(1)
					! returns(end)
					! branch(0)
      if (gtval .ne. 48) then		! count(1)
					! branch(end)
					! branch(end)
         print *,"gtall part 3:  ", gtval, 48
         gtall = 0
      end if

      gtval = 0				! count(1)
					! returns(100)
      gtval = gtval + gt4(1)		! count(1)
					! returns(100)
      gtval = gtval + gt4(2)		! count(1)
					! returns(100)
      gtval = gtval + gt4(3)		! count(1)
					! returns(end)
					! branch(0)
      if (gtval .ne. 14) then		! count(1)
					! branch(end)
         print *,"gtall part 4:  ", gtval, 14
         gtall = 0
      end if

      gtval = 0				! count(1)
					! returns(100)
      gtval = gtval + gt5(0)		! count(1)
					! returns(100)
      gtval = gtval + gt5(-1)		! count(1)
					! returns(100)
      gtval = gtval + gt5(5)		! count(1)
					! returns(end)
					! branch(0)
      if (gtval .ne. 14) then		! count(1)
					! branch(end)
         print *,"gtall part 5:  ", gtval, 14
         gtall = 0
      end if
      end

C Test simple GOTO.
      integer function gt1(f)
      implicit none
      integer f
					! branch(50)
      if (f .ne. 0) goto 100		! count(2)
					! branch(end)
      gt1 = 1				! count(1)
      goto 101				! count(1)
  100 gt1 = 2				! count(1)
  101 continue				! count(2)
      end

C Test simple GOTO again, this time out of a DO loop.
      integer function gt2(f)
      implicit none
      integer f
      integer i
					! branch(95)
      do i=1,10
					! branch(end)
         if (i .eq. f) goto 100		! count(19)
      end do
      gt2 = 4				! count(1)
      goto 101				! count(1)
  100 gt2 = 8				! count(1)
  101 continue				! count(2)
      end

C Test computed GOTO.
      integer function gt3(i)
      implicit none
      integer i
      goto (101, 102, 103, 104), i	! count(2)
      gt3 = 8				! count(1)
      goto 105				! count(1)
  101 gt3 = 1024
      goto 105
  102 gt3 = 2048
      goto 105
  103 gt3 = 16				! count(1)
      goto 105				! count(1)
  104 gt3 = 4096
      goto 105
  105 gt3 = gt3 * 2			! count(2)
      end

C Test assigned GOTO.
      integer function gt4(i)
      implicit none
      integer i
      integer label
      assign 101 to label		! count(3)
      if (i .eq. 2) assign 102 to label	! count(3)
      if (i .eq. 3) assign 103 to label	! count(3)
      goto label, (101, 102, 103)	! count(3)
  101 gt4 = 1				! count(1)
      goto 104				! count(1)
  102 gt4 = 2				! count(1)
      goto 104				! count(1)
  103 gt4 = 4				! count(1)
  104 gt4 = gt4 * 2			! count(3)
      end

C Test arithmetic IF (bundled with the GOTO variants).
      integer function gt5(i)
      implicit none
      integer i
      gt5 = 1				! count(3)
					! branch(67 50)
      if (i) 101, 102, 103		! count(3)
					! branch(end)
  101 gt5 = 2				! count(1)
      goto 104				! count(1)
  102 gt5 = 4				! count(1)
      goto 104				! count(1)
  103 gt5 = 8				! count(1)
  104 continue				! count(3)
      end

C Run all of the loop tests and check results against expected values.
      integer function lpall()
      implicit none
      integer loop1, loop2
      integer loopval

      lpall = 1				! count(1)
      loopval = 0			! count(1)
					! returns(100)
      loopval = loopval + loop1(1,0)	! count(1)
					! returns(100)
      loopval = loopval + loop1(1,2)	! count(1)
					! returns(100)
      loopval = loopval + loop1(1,7)	! count(1)
					! returns(end)
      if (loopval .ne. 12) then		! count(1)
         print *,"lpall part 1:  ", loopval, 12
         lpall = 0
      end if

      loopval = 0				! count(1)
						! returns(100)
      loopval = loopval + loop2(1,0,0,0)	! count(1)
						! returns(100)
      loopval = loopval + loop2(1,1,0,0)	! count(1)
						! returns(100)
      loopval = loopval + loop2(1,1,3,0)	! count(1)
						! returns(100)
      loopval = loopval + loop2(1,1,3,1)	! count(1)
						! returns(100)
      loopval = loopval + loop2(1,3,1,5)	! count(1)
						! returns(100)
      loopval = loopval + loop2(1,3,7,3)	! count(1)
						! returns(end)
      if (loopval .ne. 87) then			! count(1)
         print *,"lpall part 2:  ", loopval, 87
         lpall = 0
      end if
      end

C Test a simple DO loop.
      integer function loop1(r,n)
      implicit none
      integer r,n,i

      loop1 = r				! count(3)
					! branch(75)
      do i=1,n
					! branch(end)
         loop1 = loop1 + 1		! count(9)
      end do
      end

C Test nested DO loops.
      integer function loop2(r, l, m, n)
      implicit none
      integer r,l,m,n
      integer i,j,k
      loop2 = r				! count(6)
					! branch(60)
      do i=1,l
					! branch(77)
         do j=1,m
					! branch(73)
            do k=1,n
					! branch(end)
               loop2 = loop2 + 1	! count(81)
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
      ieall = 1				! count(1)
      ieval = 0				! count(1)
      
      ieval = ieval + ie1(0,2)		! count(1)
      ieval = ieval + ie1(0,0)		! count(1)
      ieval = ieval + ie1(1,2)		! count(1)
      ieval = ieval + ie1(10,2)		! count(1)
      ieval = ieval + ie1(11,11)	! count(1)
      if (ieval .ne. 31) then		! count(1)
         print *,"ieall part 1:  ", ieval, 31
         ieall = 0
      end if

      ieval = 0	
      ieval = ieval + ie2(0)		! count(1)
      ieval = ieval + ie2(2)		! count(1)
      ieval = ieval + ie2(2)		! count(1)
      ieval = ieval + ie2(2)		! count(1)
      ieval = ieval + ie2(3)		! count(1)
      ieval = ieval + ie2(3)		! count(1)
      if (ieval .ne. 23) then		! count(1)
         print *,"ieall part 2:  ", ieval, 23
         ieall = 0
      end if

      ieval = 0
      ieval = ieval + ie3(11,19)	! count(1)
      ieval = ieval + ie3(25,27)	! count(1)
      ieval = ieval + ie3(11,22)	! count(1)
      ieval = ieval + ie3(11,10)	! count(1)
      ieval = ieval + ie3(21,32)	! count(1)
      ieval = ieval + ie3(21,20)	! count(1)
      ieval = ieval + ie3(1,2)		! count(1)
      ieval = ieval + ie3(32,31)	! count(1)
      ieval = ieval + ie3(3,0)		! count(1)
      ieval = ieval + ie3(0,47)		! count(1)
      ieval = ieval + ie3(65,65)	! count(1)
      if (ieval .ne. 246) then		! count(1)
         print *,"ieall part 3:  ", ieval, 246
         ieall = 0
      end if
      end

C Test IF-THEN-ELSE.
      integer function ie1(i,j)
      implicit none
      integer i,j
      integer foo

      ie1 = 0				! count(5)
					! branch(40)
      if (i .ne. 0) then		! count(5)
					! branch(0)
         if (j .ne. 0) then		! count(3)
					! branch(end)
            ie1 = foo(4)		! count(3)
         else
            ie1 = foo(1024)
         end if
      else
					! branch(50)
         if (j .ne. 0) then		! count(2)
					! branch(end)
            ie1 = foo(1)		! count(1)
         else
            ie1 = foo(2)		! count(1)
         end if
      end if
					! branch(80)
      if (i .gt. j) then		! count(5)
					! branch(end)
         ie1 = foo(ie1*2)
      end if
					! branch(80)
      if (i .gt. 10) then		! count(5)
					! branch(0)
         if (j .gt. 10) then		! count(1)
					! branch(end)
            ie1 = foo(ie1*4)		! count(1)
         end if
      end if
      end

C Test a series of simple IF-THEN statements.
      integer function ie2(i)
      implicit none
      integer i
      integer foo
      ie2 = 0				! count(6)

					! branch(83)
      if (i .eq. 0) then		! count(6)
					! branch(end)
         ie2 = foo(1)			! count(1)
      end if
					! branch(100)
      if (i .eq. 1) then		! count(6)
					! branch(end)
         ie2 = foo(1024)
      end if
					! branch(50)
      if (i .eq. 2) then		! count(6)
					! branch(end)
         ie2 = foo(2)			! count(3)
      end if
					! branch(67)
      if (i .eq. 3) then		! count(6)
					! branch(end)
         ie2 = foo(8)			! count(2)
      end if
					! branch(100)
      if (i .eq. 4) then		! count(6)
					! branch(end)
         ie2 = foo(2048)
      end if

      end

C Test nested IF statements and IF with compound expressions.
      integer function ie3(i,j)
      implicit none
      integer i,j
      integer foo

      ie3 = 1				! count(11)
					! branch(27 50 75)
      if ((i .gt. 10) .and. (j .gt. i) .and. (j .lt. 20)) then ! count(11)
					! branch(end)
         ie3 = foo(16)			! count(1)
      end if
					! branch(55)
      if (i .gt. 20) then		! count(11)
					! branch(60)
         if (j .gt. i) then		! count(5)
					! branch(50)
            if (j .lt. 30) then		! count(2)
					! branch(end)
               ie3 = foo(32)		! count(1)
            end if
         end if
      end if
					! branch(9 10 11)
      if ((i .eq. 3) .or. (j .eq. 47) .or. (i .eq.j)) then ! count(11)
					! branch(end)
         ie3 = foo(64)			! count(3)
      end if
      end
C
C { dg-final { run-gcov branches calls { -b gcov-1.f } } }
