! { dg-do run }
!
! Test the fix for PR37749 in which the expression in line 13 would cause an ICE
! because the upper value of the loop range was not set.
!
! Contributed by Jakub Jelinek <jakub@gcc.gnu.org>
!
subroutine subr (m, n, a, b, c, d, p)
  implicit none
  integer m, n
  real a(m,n), b(m,n), c(n,n), d(m,n)
  integer p(n)
  d = a(:,p) - matmul(b, c)
end subroutine

  implicit none
  integer i
  real a(3,2), b(3,2), c(2,2), d(3,2)
  integer p(2)
  a = reshape ((/(i, i = 1, 6)/), (/3, 2/))
  b = 1
  c = 2
  p = 2
  call subr (3, 2, a, b, c, d, p)
  if (any (d .ne. reshape ((/(mod (i + 2, 3), i = 1, 6)/), (/3, 2/)))) call abort
end
