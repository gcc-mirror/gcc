! { dg-do run }
!
! PR fortran/33197
!
! Check implementation of L2 norm (Euclidean vector norm)
!
implicit none

real :: a(3) = [real :: 1, 2, huge(3.0)]
real :: b(3) = [real :: 1, 2, 3]
real :: c(4) = [real :: 1, 2, 3, -1]
real :: e(0) = [real :: ]
real :: f(4) = [real :: 0, 0, 3, 0 ]

real :: d(4,1) = RESHAPE ([real :: 1, 2, 3, -1], [4,1])
real :: g(4,1) = RESHAPE ([real :: 0, 0, 4, -1], [4,1])

! Check compile-time version

if (abs (NORM2 ([real :: 1, 2, huge(3.0)])   - huge(3.0)) &
    > epsilon(0.0)*huge(3.0)) call abort()

if (abs (SNORM2([real :: 1, 2, huge(3.0)],3) - huge(3.0)) &
    > epsilon(0.0)*huge(3.0)) call abort()

if (abs (SNORM2([real :: 1, 2, 3],3) - NORM2([real :: 1, 2, 3])) &
    > epsilon(0.0)*SNORM2([real :: 1, 2, 3],3)) call abort()

if (NORM2([real :: ]) /= 0.0) call abort()
if (abs (NORM2([real :: 0, 0, 3, 0]) - 3.0) > epsilon(0.0)) call abort()

! Check TREE version

if (abs (NORM2 (a)   - huge(3.0)) &
    > epsilon(0.0)*huge(3.0)) call abort()

if (abs (SNORM2(b,3) - NORM2(b)) &
    > epsilon(0.0)*SNORM2(b,3)) call abort()

if (abs (SNORM2(c,4) - NORM2(c)) &
    > epsilon(0.0)*SNORM2(c,4)) call abort()

if (ANY (abs (abs(d(:,1)) - NORM2(d, 2)) &
    > epsilon(0.0))) call abort()

! Check libgfortran version

if (ANY (abs (SNORM2(d,4) - NORM2(d, 1)) &
    > epsilon(0.0)*SNORM2(d,4))) call abort()

if (abs (SNORM2(f,4) - NORM2(f, 1)) &
    > epsilon(0.0)*SNORM2(d,4)) call abort()

if (ANY (abs (abs(g(:,1)) - NORM2(g, 2)) &
    > epsilon(0.0))) call abort()

contains
   ! NORM2 algorithm based on BLAS, cf.
   ! http://www.netlib.org/blas/snrm2.f
   REAL FUNCTION SNORM2 (X,n)
      INTEGER, INTENT(IN) :: n
      REAL, INTENT(IN) :: X(n)

      REAL :: absXi, scale, SSQ
      INTEGER :: i

      INTRINSIC :: ABS, SQRT

      IF (N < 1) THEN
        snorm2 = 0.0
      ELSE IF (N == 1) THEN
        snorm2 = ABS(X(1))
      ELSE
          scale = 0.0
          SSQ = 1.0

          DO i = 1, N
              IF (X(i) /= 0.0) THEN
                  absXi = ABS(X(i))
                  IF (scale < absXi) THEN
                      SSQ = 1.0 + SSQ * (scale/absXi)**2
                      scale = absXi
                  ELSE
                      SSQ = SSQ + (absXi/scale)**2
                  END IF
              END IF
          END DO
          snorm2 = scale * SQRT(SSQ)
      END IF
   END FUNCTION SNORM2
end
