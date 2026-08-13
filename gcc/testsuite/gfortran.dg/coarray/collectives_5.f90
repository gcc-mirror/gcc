! { dg-do run }
!
! PR fortran/126799
! Check that the collective subroutines code uses span and element length
! correctly when they are different (in the non-contiguous pointer case).
! The shared memory implementation used to use the span incorrectly as element
! length, causing wrong values to be produced.

program prog
  implicit none
  integer, parameter :: k = 2, n = 5
  type t
    integer(kind=k) :: c1, c2
  end type
  type(t), target :: x(n)
  integer(kind=k), pointer :: p(:)
  integer :: i, icount
  icount = num_images()
  !print *, icount
  x = [ (t(i*this_image(),i+this_image()), i=1,n) ]
  p => x%c1
  call summation(p)
  !print '(i4,":",*(" ", i5))', this_image(), x%c1
  if (any(x%c1 /= [ ((icount * (icount + 1) / 2) * i, i=1,n) ])) error stop 1
  p => x%c2
  call summation(p)
  !print '(i4,":",*(" ", i5))', this_image(), x%c2
  if (any(x%c2 /= [ (icount * i + icount * (icount + 1) / 2, i=1,n) ])) error stop 2
contains
  subroutine summation(a)
    integer(kind=k), pointer, intent(in) :: a(:)
    call co_sum(a)
  end subroutine
end program
