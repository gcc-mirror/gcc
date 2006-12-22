! { dg-do run }
! Tests the patch for PR30025, aka 25818, in which the initialization
! code for the array a, was causing a segfault in runtime for a call
! to x, since n is missing.
!
! COntributed by Elizabeth Yip <elizabeth.l.yip@boeing.com>
      program test_entry
      common // j
      real a(10)
      a(1) = 999.
      call x
      if (j .ne. 1) call abort ()
      call y(a,10)
      if (j .ne. 2) call abort ()
      stop
      end 
      subroutine x
      common // j
      real a(n)
      j = 1
      return
      entry y(a,n)
      call foo(a(1))
      end
      subroutine foo(a)
      common // j
      real a
      j = 2
      return
      end

