      subroutine sgbcon(norm,n,kl,ku,ab,ldab,ipiv,anorm,rcond,work,iwork
     &,info)
C
C  -- LAPACK routine (version 3.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      character norm
      integer info,kl,ku,ldab,n
      real anorm,rcond
C     ..
C     .. Array Arguments ..
      integer ipiv(n),iwork(n)
      real ab(ldab,n),work(n)
C     ..
C
C  Purpose
C  =======
C demonstrate g77 bug at -O -funroll-loops
C  =====================================================================
C
C     .. Parameters ..
      real one,zero
      parameter(one= 1.0e+0,zero= 0.0e+0)
C     ..
C     .. Local Scalars ..
      logical lnoti,onenrm
      character normin
      integer ix,j,jp,kase,kase1,kd,lm
      real ainvnm,scale,smlnum,t
C     ..
C     .. External Functions ..
      logical lsame
      integer isamax
      real sdot,slamch
      externallsame,isamax,sdot,slamch
C     ..
C     .. External Subroutines ..
      externalsaxpy,slacon,slatbs,srscl,xerbla
C     ..
C     .. Executable Statements ..
C
C           Multiply by inv(L).
C
      do j= 1,n-1
C the following min() intrinsic provokes this bug
	  lm= min(kl,n-j)
	  jp= ipiv(j)
	  t= work(jp)
	  if(jp.ne.j)then
C but only when combined with this if block
	      work(jp)= work(j)
	      work(j)= t
	    endif
C and this subroutine call
	  call saxpy(lm,-t,ab(kd+1,j),1,work(j+1),1)
	enddo
      return
      end
