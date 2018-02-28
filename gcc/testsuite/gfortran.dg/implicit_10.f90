! { dg-do run }
! Check fix for PR24783 where we did try to resolve the implicit type
! from the wrong namespace thus rejecting valid code.
      MODULE mod1
      IMPLICIT NONE
      CONTAINS
      SUBROUTINE sub(vec, ny)
      IMPLICIT REAL (a-h,o-z)
      IMPLICIT INTEGER (i-n)
      DIMENSION vec(ny)
      ny = fun(vec(ny),1,1) 
      RETURN
      END SUBROUTINE sub
      REAL FUNCTION fun(r1, i1, i2)
      IMPLICIT REAL (r,f)
      IMPLICIT INTEGER (i)
      DIMENSION r1(i1:i2)
      r1(i1) = i1 + 1
      r1(i2) = i2 + 1
      fun = r1(i1) + r1(i2)
      END FUNCTION fun
      END MODULE mod1

      use mod1
      IMPLICIT REAL (d)
      INTEGER i
      dimension di(5)
      i = 1
      if (fun(di(i),1,2).NE.5) STOP 1
      call sub(di(i),i)
      if (i.NE.4) STOP 2
      end
