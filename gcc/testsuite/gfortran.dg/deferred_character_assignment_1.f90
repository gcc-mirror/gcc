! { dg-do run }
!
! Checks the fix for PR67977 in which automatic reallocation on assignment
! was performed when the lhs had a substring reference.
!
! Contributed by Anton Shterenlikht  <mexas@bristol.ac.uk>
!
  character(:), allocatable :: z
  integer :: length
  z = "cockatoo"
  length = len (z)
  z(:) = ''
  if (len(z) .ne. length) STOP 1
  if (trim (z) .ne. '') STOP 2
  z(:3) = "foo"
  if (len(z) .ne. length) STOP 3
  if (trim (z) .ne. "foo") STOP 4
  z(4:) = "__bar"
  if (len(z) .ne. length) STOP 5
  if (trim (z) .ne. "foo__bar") STOP 6
  deallocate (z)
end
