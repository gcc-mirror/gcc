! { dg-do run }
! Tests the fix for the bug PR30746, in which the reference to 'x'
! in 'inner' wrongly host-associated with the variable 'x' rather
! than the function.
!
! Testcase is due to Malcolm Cohen, NAG.
!
real function z (i)
  integer :: i
  z = real (i)**i
end function

MODULE m
  REAL :: x(3) = (/ 1.5, 2.5, 3.5 /)
  interface
    real function z (i)
      integer :: i
    end function
  end interface
CONTAINS
  SUBROUTINE s
    if (x(2, 3) .ne. real (2)**3) call abort ()
    if (z(3, 3) .ne. real (3)**3) call abort ()
    CALL inner
  CONTAINS
    SUBROUTINE inner
      i = 7
      if (x(i, 7) .ne. real (7)**7) call abort ()
      if (z(i, 7) .ne. real (7)**7) call abort ()
    END SUBROUTINE
    FUNCTION x(n, m)
      x = REAL(n)**m
    END FUNCTION
    FUNCTION z(n, m)
      z = REAL(n)**m
    END FUNCTION

  END SUBROUTINE
END MODULE
  use m
  call s()
end
! { dg-final { cleanup-modules "m" } }
