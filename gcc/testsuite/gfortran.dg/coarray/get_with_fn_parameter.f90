!{ dg-do run }

! Check that non-pure/non-elemental functions in caf(fn(..))[..]
! are outlined to be called on this image.

program get_with_fn_parameter

  implicit none

  integer, allocatable :: caf(:)[:]
  integer, parameter :: i = 10
  integer :: j

  allocate(caf(i)[*], source = (/(j, j= 1, 10 )/))
  if (any(caf(fn(i))[1] /= fn(i))) stop 1
  deallocate(caf)

contains

function fn(n)
  integer, intent(in) :: n
  integer :: fn(n)
  integer :: i

  fn = (/(i, i = 1, n)/)
end function

end program

