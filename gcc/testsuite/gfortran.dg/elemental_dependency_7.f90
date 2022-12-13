! { dg-do run }
! PR fortran/107819 - ICE in gfc_check_argument_var_dependency
! Contributed by G.Steinmetz
!
! Note: the testcase is considered non-conforming for m>1 due to aliasing

program p
  implicit none
  integer, parameter :: m = 1
  integer :: i
  integer :: a(m) = [(-i,i=1,m)]
  integer :: n(m) = [(i,i=m,1,-1)]
  integer :: b(m)
  b = a
  call s (a(n), a) ! { dg-warning "might interfere with actual argument" }

  ! Compare to separate application of subroutine in element order
  do i = 1, size (b)
     call s (b(n(i)), b(i))
  end do
  if (any (a /= b)) stop 1
contains
  elemental subroutine s (x, y)
    integer, value       :: x
    integer, intent(out) :: y
    y = x
  end
end
