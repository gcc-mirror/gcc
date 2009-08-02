c { dg-do run }
c { dg-options "-std=legacy" }
c
* From: "Billinghurst, David (RTD)" <David.Billinghurst@riotinto.com.au>
* Subject: RE: single precision complex bug in g77 - was Testing g77 with LA
* PACK 3.0
* Date: Thu, 8 Jul 1999 00:55:11 +0100 
* X-UIDL: b00d9d8081a36fef561b827d255dd4a5

* Here is a slightly simpler and neater test case

      program labug3
      implicit none

*  This program gives the wrong answer on mips-sgi-irix6.5
*  when compiled with g77 from egcs-19990629 (gcc 2.95 prerelease)
*  Get a = 0.0 when it should be 1.0 
*
*  Works with:  -femulate-complex
*               egcs-1.1.2 
*
*  Originally derived from LAPACK 3.0 test suite.
*
*  David Billinghurst, (David.Billinghurst@riotinto.com.au)
*  8 July 1999
* 
      complex one, z
      real    a, f1
      f1(z) = real(z)
      one = (1.,0.)
      a = f1(one) 
      if ( abs(a-1.0) .gt. 1.0e-5 ) then
         write(6,*) 'A should be 1.0 but it is',a
         call abort()
      end if
      end
