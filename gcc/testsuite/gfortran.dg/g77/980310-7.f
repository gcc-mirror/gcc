c { dg-do compile }
C From: "David C. Doherty" <doherty@networkcs.com>
C Message-Id: <199711171846.MAA27947@uh.msc.edu>
C Subject: g77: auto arrays + goto = no go
C To: egcs-bugs@cygnus.com
C Date: Mon, 17 Nov 1997 12:46:27 -0600 (CST)

C I sent the following to fortran@gnu.ai.mit.edu, and Dave Love
C replied that he was able to reproduce it on rs6000-aix; not on
C others. He suggested that I send it to egcs-bugs. 

C Hi - I've observed the following behavior regarding 
C automatic arrays and gotos.  Seems similar to what I found
C in the docs about computed gotos (but not exactly the same).
C 
C I suspect from the nature of the error msg that it's in the GBE.
C 
C I'm using egcs-971105, under linux-ppc.
C 
C I also observed the same in g77-0.5.19 (and gcc 2.7.2?).
C 
C I'd appreciate any advice on this.  thanks for the great work.
C --
C >cat testg77.f
      subroutine testg77(n, a)
c
      implicit none
c
      integer n
      real a(n)
      real b(n)
      integer i
c
      do i = 1, 10
        if (i .gt. 4) goto 100
        write(0, '(i2)')i
      enddo
c
      goto 200
100   continue
200   continue
c
      return
      end
C >g77 -c testg77.f
C testg77.f: In subroutine `testg77':
C testg77.f:19: label `200' used before containing binding contour
C testg77.f:18: label `100' used before containing binding contour
C --
C If I comment out the b(n) line or replace it with, e.g., b(10),
C it compiles fine.
