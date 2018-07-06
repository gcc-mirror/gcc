! { dg-do run }
! { dg-require-effective-target fortran_large_real }
!
!
! PR fortran/33197
!
! Check implementation of L2 norm (Euclidean vector norm)
!
implicit none

integer,parameter :: qp = selected_real_kind (precision (0.0d0)+1)

real(qp) :: a(3) = [real(qp) :: 1, 2, huge(3.0_qp)]
real(qp) :: b(3) = [real(qp) :: 1, 2, 3]
real(qp) :: c(4) = [real(qp) :: 1, 2, 3, -1]
real(qp) :: e(0) = [real(qp) :: ]
real(qp) :: f(4) = [real(qp) :: 0, 0, 3, 0 ]

real(qp) :: d(4,1) = RESHAPE ([real(qp) :: 1, 2, 3, -1], [4,1])
real(qp) :: g(4,1) = RESHAPE ([real(qp) :: 0, 0, 4, -1], [4,1])

! Check compile-time version

if (abs (NORM2 ([real(qp) :: 1, 2, huge(3.0_qp)])   - huge(3.0_qp)) &
    > epsilon(0.0_qp)*huge(3.0_qp)) STOP 1

if (abs (SNORM2([real(qp) :: 1, 2, huge(3.0_qp)],3) - huge(3.0_qp)) &
    > epsilon(0.0_qp)*huge(3.0_qp)) STOP 2

if (abs (SNORM2([real(qp) :: 1, 2, 3],3) - NORM2([real(qp) :: 1, 2, 3])) &
    > epsilon(0.0_qp)*SNORM2([real(qp) :: 1, 2, 3],3)) STOP 3

if (NORM2([real(qp) :: ]) /= 0.0_qp) STOP 4
if (abs (NORM2([real(qp) :: 0, 0, 3, 0]) - 3.0_qp) > epsilon(0.0_qp)) STOP 5

! Check TREE version

if (abs (NORM2 (a)   - huge(3.0_qp)) &
    > epsilon(0.0_qp)*huge(3.0_qp)) STOP 6

if (abs (SNORM2(b,3) - NORM2(b)) &
    > epsilon(0.0_qp)*SNORM2(b,3)) STOP 7

if (abs (SNORM2(c,4) - NORM2(c)) &
    > epsilon(0.0_qp)*SNORM2(c,4)) STOP 8

if (ANY (abs (abs(d(:,1)) - NORM2(d, 2)) &
    > epsilon(0.0_qp))) STOP 9

! Check libgfortran version

if (ANY (abs (SNORM2(d,4) - NORM2(d, 1)) &
    > epsilon(0.0_qp)*SNORM2(d,4))) STOP 10

if (abs (SNORM2(f,4) - NORM2(f, 1)) &
    > epsilon(0.0_qp)*SNORM2(d,4)) STOP 11

if (ANY (abs (abs(g(:,1)) - NORM2(g, 2)) &
    > epsilon(0.0_qp))) STOP 12

contains
   ! NORM2 algorithm based on BLAS, cf.
   ! http://www.netlib.org/blas/snrm2.f
   REAL(qp) FUNCTION SNORM2 (X,n)
      INTEGER, INTENT(IN) :: n
      REAL(qp), INTENT(IN) :: X(n)

      REAL(qp) :: absXi, scale, SSQ
      INTEGER :: i

      INTRINSIC :: ABS, SQRT

      IF (N < 1) THEN
        snorm2 = 0.0_qp
      ELSE IF (N == 1) THEN
        snorm2 = ABS(X(1))
      ELSE
          scale = 0.0_qp
          SSQ = 1.0_qp

          DO i = 1, N
              IF (X(i) /= 0.0_qp) THEN
                  absXi = ABS(X(i))
                  IF (scale < absXi) THEN
                      SSQ = 1.0_qp + SSQ * (scale/absXi)**2
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
