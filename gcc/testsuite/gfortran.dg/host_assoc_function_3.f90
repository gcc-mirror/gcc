! { dg-do run }
! Tests the fix for the bug PR33233, in which the reference to 'x'
! in 'inner' wrongly host-associated with the variable 'x' rather
! than the function.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
MODULE m
  REAL :: x(3) = (/ 1.5, 2.5, 3.5 /)
CONTAINS
  SUBROUTINE s
    if (x(2) .eq. 2.5) call abort ()
  CONTAINS
    FUNCTION x(n, m)
      integer, optional :: m
      if (present(m)) then
        x = REAL(n)**m
      else
        x = 0.0
      end if
    END FUNCTION
  END SUBROUTINE s
END MODULE m
  use m
  call s
end
