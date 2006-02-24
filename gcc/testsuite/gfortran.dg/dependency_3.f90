! { dg-do run }
! Tests the fix for PR24519, in which assignments with the same
! range of an assumed shape array, on the lhs and rhs, would be
! treated as causing a dependency.
!
! Contributed by Paul.Thomas  <pault@gcc.gnu.org>
!
  integer, parameter :: n = 100
  real :: x(n, n), v
  x = 1
  v = 0.1
  call foo (x, v)
  if (abs(sum (x) -  91.10847) > 1e-3) print *, sum (x)
contains
  subroutine foo (b, d)
    real :: b(:, :)
    real :: temp(n), c, d
    integer :: j, k
    do k = 1, n
      temp = b(:,k)
      do j = 1, n
        c = b(k,j)*d
        b(:,j) = b(:,j)-temp*c  ! This was the offending assignment.
        b(k,j) = c
      end do
    end do
  end subroutine foo
end
