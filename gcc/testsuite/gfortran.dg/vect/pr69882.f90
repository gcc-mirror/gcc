! { dg-additional-options "-Ofast" }
! { dg-additional-options "-mavx" { target avx_runtime } }

subroutine foo(a, x)
  implicit none

  integer, parameter :: XX=4, YY=26
  integer, intent(in) :: x
  real *8, intent(in) :: a(XX,YY)
  real *8 :: c(XX)

  integer i, k

  c = 0

  do k=x,YY
     do i=1,2
        c(i) = max(c(i), a(i,k))
     end do
  end do

  PRINT *, "c=", c

  IF (c(1) .gt. 0.0) THEN
     CALL ABORT
  END IF

  IF (c(2) .gt. 0.0) THEN
     CALL ABORT
  END IF
end subroutine foo

PROGRAM MAIN
  real *8 a(4, 26)

  a = 0
  a(3,1) = 100.0
  a(4,1) = 100.0
  
  CALL FOO(a, 1)
END PROGRAM
