! { dg-do run }
!
! Test the fix for PR91686
!
! Contributed by  <urbanjost@comcast.net>
!
program shuf
  implicit none
  character(len=:),allocatable :: pageout(:)
  integer                      :: i
  pageout=[character(len=20) :: 'a','bbbbbbb','ccccc']
  pageout=pageout([3,2,1])
  if (trim( pageout(1)) .ne. 'ccccc') stop 1
  if (trim( pageout(2)) .ne. 'bbbbbbb') stop 2
  if (trim( pageout(3)) .ne. 'a') stop 3
end program shuf
