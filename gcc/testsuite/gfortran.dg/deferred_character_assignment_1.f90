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
  if (len(z) .ne. length) call abort
  if (trim (z) .ne. '') call abort
  z(:3) = "foo"
  if (len(z) .ne. length) call abort
  if (trim (z) .ne. "foo") call abort
  z(4:) = "__bar"
  if (len(z) .ne. length) call abort
  if (trim (z) .ne. "foo__bar") call abort
  deallocate (z)
end
