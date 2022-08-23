! { dg-do run }
!
! Test the fix for PR70673
!
! Contributed by David Kinniburgh  <davidgkinniburgh@yahoo.co.uk>
!
module m
contains
  subroutine s(inp)
    character(*), intent(in) :: inp
    character(:), allocatable :: a
    a = inp
    a = a           ! This used to ICE too
    if ((len (a) .ne. 5) .or. (a .ne. "hello")) STOP 1
    a = a(2:3)      ! Make sure that temporary creation is not broken.
    if ((len (a) .ne. 2) .or. (a .ne. "el")) STOP 2
    deallocate (a)
  end subroutine s
end module m

  use m
  call s("hello")
end
