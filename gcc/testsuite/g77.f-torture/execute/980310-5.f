C Confirmed on EGCS 1.0.1 on i586-pc-sco3.2v5.0.4
C To: egcs-bugs@cygnus.com
C Subject: [Vladimir Eltsov <ve@boojum.hut.fi>] bug with -fcaller-saves
C From: Dave Love <d.love@dl.ac.uk>
C Date: 29 Jan 1998 18:20:47 +0000
C Message-ID: <rzq67n3cfb4.fsf@djlvig.dl.ac.uk>

C This appears to be a (non-critical?) backend problem reported as a g77
C bug.  I can reproduce it, but (only) with -O[2].  Any ideas other than
C `don't do that, then'? :-)
C 
C ------- Start of forwarded message -------
C Date: Tue, 27 Jan 1998 19:25:19 +0200 (EET)
C From: Vladimir Eltsov <ve@boojum.hut.fi>
C To: fortran@gnu.org
C Subject: bug with -fcaller-saves
C Message-ID: <Pine.LNX.3.96.980127190257.1606A-100000@slon.hut.fi>
C MIME-Version: 1.0
C Content-Type: TEXT/PLAIN; charset=US-ASCII
C 
C Hello!
C 
C Following program would hang after printing 6 lines when compiled with 
C 'g77 -O2 test.f' on x86 architecture, but would work OK when compiled with 
C 'g77 -O2 -fno-caller-saves test.f' both for gnu and egcs variants of the
C compiler.
C 
C Details follow:
C -------  test.f -------
      program test
      implicit double precision (a-h,o-z)

      t = 0
C 	Was: tend=1.  Changed to shorten runtime. robertl
      tend = .0320d-3
      dt = 6d-7
      h = 0.314d-7
      k = 1
      ti = dt

      do while (t.lt.tend)
         do while(t.lt.ti)
            if (t+h.gt.ti) then
               h = ti-t
            end if
            call fun(t,h)
         end do
         print *,k,t,t/5d-7
         k = k+1
         ti = k*dt
      end do

      end

      subroutine fun(t,h)
      implicit double precision (a-h,o-z)

      t = t+h
      h = 0.314d-7

      return
      end
