! { dg-do run }
! { dg-options "-pedantic-errors -mdalign" { target sh*-*-* } }
! Tests the fix for PR37614, in which the alignment of commons followed
! g77 rather than the standard or other compilers.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
subroutine foo (z)
  real(8) x, y, z
  common i(8)
  equivalence (x, i(3)),(y,i(7))
  if ((i(1) .ne. 42) .or. (i(5) .ne. 43)) call abort
  if ((i(2) .ne. 0) .or. (i(2) .ne. 0)) call abort
  if ((x .ne. z) .or. (y .ne. z)) call abort
end subroutine

subroutine bar
  common i(8)
  i = 0
end subroutine

  real(8) x, y
  common i, x, j, y ! { dg-warning "Padding" }
  call bar
  i = 42
  j = 43
  x = atan (1.0)*4.0
  y = x
  call foo (x)
end

