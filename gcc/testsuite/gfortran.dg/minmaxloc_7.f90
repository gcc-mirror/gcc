! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
! { dg-do run }
program test
  implicit none
  real, volatile, allocatable :: A(:)
  logical, volatile :: mask(11)

  A = [1,2,3,5,6,1,35,3,7,-3,-47]
  mask = .true.
  mask(7) = .false.
  mask(11) = .false.
  call sub2 (minloc(A),11)
  call sub2 (maxloc(A, mask=mask),9)
  A = minloc(A)
  if (size (A) /= 1 .or. A(1) /= 11) call abort ()
contains
  subroutine sub2(A,n)
    integer :: A(:),n
    if (A(1) /= n .or. size (A) /= 1) call abort ()
  end subroutine sub2
end program test
