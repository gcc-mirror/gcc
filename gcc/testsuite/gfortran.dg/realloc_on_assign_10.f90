! { dg-do run }
! PR52012 - with realloc_lhs active(ie. default condition) the
! offset was wrongly calculated for a, after assignment.
!
! Reported by Reinhold Bader and Tobias Burnus  <burnus@gcc.gnu.org>
! 
program gf
  implicit none
  real, allocatable :: a(:,:,:)
  real, parameter :: zero = 0.0, one = 1.0
  real :: b(3,4,5) = zero
  b(1,2,3) = one
  allocate (a(size (b, 3), size (b, 2), size (b, 1)))
  a = reshape (b, shape (a), order = [3, 2, 1])
  if (any (a(:, 2, 1) .ne. [zero, zero, one, zero, zero])) STOP 1
  if (a(3, 2, 1) /= one) STOP 1
  if (sum (abs (a)) /= one) STOP 2
end program
