!{ dg-do run }

! Check that non-pure/non-elemental functions in caf(fn(..))[..]
! are outlined to be called on this image.

program get_with_fn_parameter

  implicit none

  integer, allocatable :: caf(:)[:]
  integer, parameter :: i = 10
  integer :: n

  allocate(caf(i)[*], source =(/(n, n = i, 1, -1)/))
  do n = 1, i
    if (caf(pivot(n))[1] /= i - pivot(n) + 1) stop n
  end do
  deallocate(caf)

contains

function pivot(n)
  integer, intent(in) :: n
  integer :: pivot

  pivot = i - n + 1
end function

end program

