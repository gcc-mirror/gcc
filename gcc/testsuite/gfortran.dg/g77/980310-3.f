c { dg-do compile }
c
c This demonstrates a problem with g77 and pic on x86 where 
c egcs 1.0.1 and earlier will generate bogus assembler output.
c unfortunately, gas accepts the bogus acssembler output and 
c generates code that almost works.
c


C Date: Wed, 17 Dec 1997 23:20:29 +0000
C From: Joao Cardoso <jcardoso@inescn.pt>
C To: egcs-bugs@cygnus.com
C Subject: egcs-1.0 f77 bug on OSR5
C When trying to compile the Fortran file that I enclose bellow,
C I got an assembler error:
C 
C ./g77 -B./ -fpic -O -c scaleg.f
C /usr/tmp/cca002D8.s:123:syntax error at (
C 
C ./g77 -B./ -fpic -O0 -c scaleg.f 
C /usr/tmp/cca002EW.s:246:invalid operand combination: leal
C 
C Compiling without the -fpic flag runs OK.

      subroutine scaleg (n,ma,a,mb,b,low,igh,cscale,cperm,wk)
c
c     *****parameters:
      integer igh,low,ma,mb,n
      double precision a(ma,n),b(mb,n),cperm(n),cscale(n),wk(n,6)
c
c     *****local variables:
      integer i,ir,it,j,jc,kount,nr,nrp2
      double precision alpha,basl,beta,cmax,coef,coef2,coef5,cor,
     *                 ew,ewc,fi,fj,gamma,pgamma,sum,t,ta,tb,tc
c
c     *****fortran functions:
      double precision dabs, dlog10, dsign
c     float
c
c     *****subroutines called:
c     none
c
c     ---------------------------------------------------------------
c
c     *****purpose:
c     scales the matrices a and b in the generalized eigenvalue
c     problem a*x = (lambda)*b*x such that the magnitudes of the
c     elements of the submatrices of a and b (as specified by low
c     and igh) are close to unity in the least squares sense.
c     ref.:  ward, r. c., balancing the generalized eigenvalue
c     problem, siam j. sci. stat. comput., vol. 2, no. 2, june 1981,
c     141-152.
c
c     *****parameter description:
c
c     on input:
c
c       ma,mb   integer
c               row dimensions of the arrays containing matrices
c               a and b respectively, as declared in the main calling
c               program dimension statement;
c
c       n       integer
c               order of the matrices a and b;
c
c       a       real(ma,n)
c               contains the a matrix of the generalized eigenproblem
c               defined above;
c
c       b       real(mb,n)
c               contains the b matrix of the generalized eigenproblem
c               defined above;
c
c       low     integer
c               specifies the beginning -1 for the rows and
c               columns of a and b to be scaled;
c
c       igh     integer
c               specifies the ending -1 for the rows and columns
c               of a and b to be scaled;
c
c       cperm   real(n)
c               work array.  only locations low through igh are
c               referenced and altered by this subroutine;
c
c       wk      real(n,6)
c               work array that must contain at least 6*n locations.
c               only locations low through igh, n+low through n+igh,
c               ..., 5*n+low through 5*n+igh are referenced and
c               altered by this subroutine.
c
c     on output:
c
c       a,b     contain the scaled a and b matrices;
c
c       cscale  real(n)
c               contains in its low through igh locations the integer
c               exponents of 2 used for the column scaling factors.
c               the other locations are not referenced;
c
c       wk      contains in its low through igh locations the integer
c               exponents of 2 used for the row scaling factors.
c
c     *****algorithm notes:
c     none.
c
c     *****history:
c     written by r. c. ward.......
c     modified 8/86 by bobby bodenheimer so that if
c       sum = 0 (corresponding to the case where the matrix
c       doesn't need to be scaled) the routine returns.
c
c     ---------------------------------------------------------------
c
      if (low .eq. igh) go to 410
      do 210 i = low,igh
         wk(i,1) = 0.0d0
         wk(i,2) = 0.0d0
         wk(i,3) = 0.0d0
         wk(i,4) = 0.0d0
         wk(i,5) = 0.0d0
         wk(i,6) = 0.0d0
         cscale(i) = 0.0d0
         cperm(i) = 0.0d0
  210 continue
c
c     compute right side vector in resulting linear equations
c
      basl = dlog10(2.0d0)
      do 240 i = low,igh
         do 240 j = low,igh
            tb = b(i,j)
            ta = a(i,j)
            if (ta .eq. 0.0d0) go to 220
            ta = dlog10(dabs(ta)) / basl
  220       continue
            if (tb .eq. 0.0d0) go to 230
            tb = dlog10(dabs(tb)) / basl
  230       continue
            wk(i,5) = wk(i,5) - ta - tb
            wk(j,6) = wk(j,6) - ta - tb
  240 continue
      nr = igh-low+1
      coef = 1.0d0/float(2*nr)
      coef2 = coef*coef
      coef5 = 0.5d0*coef2
      nrp2 = nr+2
      beta = 0.0d0
      it = 1
c
c     start generalized conjugate gradient iteration
c
  250 continue
      ew = 0.0d0
      ewc = 0.0d0
      gamma = 0.0d0
      do 260 i = low,igh
         gamma = gamma + wk(i,5)*wk(i,5) + wk(i,6)*wk(i,6)
         ew = ew + wk(i,5)
         ewc = ewc + wk(i,6)
  260 continue
      gamma = coef*gamma - coef2*(ew**2 + ewc**2)
     +        - coef5*(ew - ewc)**2
      if (it .ne. 1) beta = gamma / pgamma
      t = coef5*(ewc - 3.0d0*ew)
      tc = coef5*(ew - 3.0d0*ewc)
      do 270 i = low,igh
         wk(i,2) = beta*wk(i,2) + coef*wk(i,5) + t
         cperm(i) = beta*cperm(i) + coef*wk(i,6) + tc
  270 continue
c
c     apply matrix to vector
c
      do 300 i = low,igh
         kount = 0
         sum = 0.0d0
         do 290 j = low,igh
            if (a(i,j) .eq. 0.0d0) go to 280
            kount = kount+1
            sum = sum + cperm(j)
  280       continue
            if (b(i,j) .eq. 0.0d0) go to 290
            kount = kount+1
            sum = sum + cperm(j)
  290    continue
         wk(i,3) = float(kount)*wk(i,2) + sum
  300 continue
      do 330 j = low,igh
         kount = 0
         sum = 0.0d0
         do 320 i = low,igh
            if (a(i,j) .eq. 0.0d0) go to 310
            kount = kount+1
            sum = sum + wk(i,2)
  310       continue
            if (b(i,j) .eq. 0.0d0) go to 320
            kount = kount+1
            sum = sum + wk(i,2)
  320    continue
         wk(j,4) = float(kount)*cperm(j) + sum
  330 continue
      sum = 0.0d0
      do 340 i = low,igh
         sum = sum + wk(i,2)*wk(i,3) + cperm(i)*wk(i,4)
  340 continue
      if(sum.eq.0.0d0) return
      alpha = gamma / sum
c
c     determine correction to current iterate
c
      cmax = 0.0d0
      do 350 i = low,igh
         cor = alpha * wk(i,2)
         if (dabs(cor) .gt. cmax) cmax = dabs(cor)
         wk(i,1) = wk(i,1) + cor
         cor = alpha * cperm(i)
         if (dabs(cor) .gt. cmax) cmax = dabs(cor)
         cscale(i) = cscale(i) + cor
  350 continue
      if (cmax .lt. 0.5d0) go to 370
      do 360 i = low,igh
         wk(i,5) = wk(i,5) - alpha*wk(i,3)
         wk(i,6) = wk(i,6) - alpha*wk(i,4)
  360 continue
      pgamma = gamma
      it = it+1
      if (it .le. nrp2) go to 250
c
c     end generalized conjugate gradient iteration
c
  370 continue
      do 380 i = low,igh
         ir = wk(i,1) + dsign(0.5d0,wk(i,1))
         wk(i,1) = ir
         jc = cscale(i) + dsign(0.5d0,cscale(i))
         cscale(i) = jc
  380 continue
c
c     scale a and b
c
      do 400 i = 1,igh
         ir = wk(i,1)
         fi = 2.0d0**ir
         if (i .lt. low) fi = 1.0d0
         do 400 j =low,n
            jc = cscale(j)
            fj = 2.0d0**jc
            if (j .le. igh) go to 390
            if (i .lt. low) go to 400
            fj = 1.0d0
  390       continue
            a(i,j) = a(i,j)*fi*fj
            b(i,j) = b(i,j)*fi*fj
  400 continue
  410 continue
      return
c
c     last line of scaleg
c
      end
