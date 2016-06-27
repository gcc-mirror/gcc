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
    a = a           ! This used to ICE.
    a = inp
    a = a           ! This used to ICE too
    if ((len (a) .ne. 5) .or. (a .ne. "hello")) call abort
    a = a(2:3)      ! Make sure that temporary creation is not broken.
    if ((len (a) .ne. 2) .or. (a .ne. "el")) call abort
    deallocate (a)
    a = a           ! This would ICE too.
  end subroutine s
end module m

  use m
  call s("hello")
end
