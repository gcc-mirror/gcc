! { dg-do run }
!
! Test the fix for PR71880 in which the string length for 'p'
! was not set for the pointer assignment.
!
! Contributed by Valery Weber  <valeryweber@hotmail.com>
!
program t
  character(:), dimension(:), allocatable, target :: c
  character(:), dimension(:), pointer :: p => NULL ()
  allocate(c, source = ['ABC','DEF','GHI'])
  p => c
  if (len(p) .ne. len (c)) stop 1
  if (size (p, 1) .ne. size (c, 1)) stop 2
  if (any (p .ne. c)) stop 3
end program t
