! { dg-do run }
! Tests the fix for PR33554, in which the default initialization
! of temp, in construct_temp, caused a segfault because it was
! being done before the array offset and lower bound were
! available.
!
! Contributed by Harald Anlauf <anlauf@gmx.de> 
!
module gfcbug72
  implicit none

  type t_datum
    character(len=8) :: mn = 'abcdefgh'
  end type t_datum

  type t_temp
    type(t_datum) :: p
  end type t_temp

contains

  subroutine setup ()
    integer :: i
    type (t_temp), pointer :: temp(:) => NULL ()

    do i=1,2
       allocate (temp (2))
       call construct_temp (temp)
       if (any (temp % p% mn .ne. 'ijklmnop')) STOP 1
       deallocate (temp)
    end do
  end subroutine setup
  !--
  subroutine construct_temp (temp)
    type (t_temp), intent(out) :: temp (:)
    if (any (temp % p% mn .ne. 'abcdefgh')) STOP 2
    temp(:)% p% mn = 'ijklmnop'
  end subroutine construct_temp
end module gfcbug72

program test
  use gfcbug72
  implicit none
  call setup ()
end program test
