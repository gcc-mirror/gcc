* X-Delivered: at request of burley on mescaline.gnu.org
* Date: Sat, 31 Oct 1998 18:26:29 +0200 (EET)
* From: "B. Yanchitsky" <yan@im.imag.kiev.ua>
* To: fortran@gnu.org
* Subject: Bug report
* MIME-Version: 1.0
* Content-Type: TEXT/PLAIN; charset=US-ASCII
* 
* There is a trouble with g77 on Alpha.
* My configuration: 
* Digital Personal Workstation 433au,
* Digital Unix 4.0D,
* GNU Fortran 0.5.23 and GNU C 2.8.1.
* 
* The following program treated successfully but crashed when running. 
* 
* C --- PROGRAM BEGIN -------
* 
      subroutine sub(N,u)
      integer N
      double precision u(-N:N,-N:N)

C vvvv    CRASH HERE   vvvvv   
      u(-N,N)=0d0
      return
      end


      program bug
      integer N
      double precision a(-10:10,-10:10)
      data a/441*1d0/
      N=10
      call sub(N,a)
      if (a(-N,N) .ne. 0d0) call abort
      end
* 
* C --- PROGRAM END -------
* 
* Good luck!
