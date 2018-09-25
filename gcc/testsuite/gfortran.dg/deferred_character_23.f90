! { dg-do run }
!
! Tests the fix for PR85603.
!
! Contributed by Walt Spector  <w6ws@earthlink.net>
!
program strlen_bug
  implicit none

  character(:), allocatable :: strings(:)
  integer :: maxlen

  strings = [ character(32) ::  &
      'short',  &
      'somewhat longer' ]
  maxlen = maxval (len_trim (strings))
  if (maxlen .ne. 15) stop 1
  strings = strings(:)(:maxlen) ! Used to ICE
  if (any (strings .ne. ['short          ','somewhat longer'])) stop 2

  deallocate (strings)          ! To check for memory leaks
end program
