!{ dg-do run }

! Check that complex numbers in coarrays can get assigned to.
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>

program pr108233
  implicit none
  complex :: c = (3.0,4.0), z[*] = (6.0, 7.0)
  complex, allocatable ::   y[:]
  allocate (y[*])
  y = c                 ! allocatable complex scalar coarray is OK
  if (c /= y) error stop 1
  z = c                 ! non-allocatable complex scalar coarray was bad
  if (c /= z) error stop 2
  call bcast_scalar  (z, c) ! failed too
  if (c /= z) error stop 3
  call assign_scalar (z, c) ! this works
  if (c /= z) error stop 4
contains
  subroutine assign_scalar (out, in)
    complex, intent(out) :: out
    complex, intent(in)  :: in
    out = in
  end subroutine assign_scalar
  subroutine bcast_scalar (out, in)
    complex, intent(out) :: out[*]
    complex, intent(in)  :: in
    out = in
  end subroutine bcast_scalar
end

