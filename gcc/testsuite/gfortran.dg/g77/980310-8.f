c { dg-do compile }
C To: egcs-bugs@cygnus.com
C Subject: egcs-g77 and array indexing
C Reply-To: etseidl@jutland.ca.sandia.gov
C Date: Wed, 26 Nov 1997 10:38:27 -0800
C From: Edward Seidl <etseidl@jutland.ca.sandia.gov>
C
C I have some horrible spaghetti code I'm trying compile with egcs-g77,
C but it's puking on code like the example below.  I have no idea if it's
C legal fortran or not, and I'm in no position to change it.  All I do know
C is it compiles with a number of other compilers, including f2c and
C g77-0.5.19.1/gcc-2.7.2.1.  When I try to compile with egcs-2.90.18 971122
C I get the following (on both i686-pc-linux-gnu and
C alphaev56-unknown-linux-gnu):
C
Cfoo.f: In subroutine `foobar':
Cfoo.f:11: 
C         subroutine foobar(norb,nnorb)
C                           ^
CArray `norb' at (^) is too large to handle

      program foo
      implicit integer(A-Z)
      dimension norb(6)
      nnorb=6

      call foobar(norb,nnorb)

      stop
      end

      subroutine foobar(norb,nnorb)
      implicit integer(A-Z)
      dimension norb(-1:*)

      do 10 i=-1,nnorb-2
        norb(i) = i+999
  10  continue

      return
      end
