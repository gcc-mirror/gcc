! { dg-do run }
!
! Check that pr 70697 is fixed.

program event_4
  use iso_fortran_env
  integer :: nc(1)
  type(event_type) done[*]
  nc(1) = 1
  event post(done[1])
  event wait(done,until_count=nc(1))
end
