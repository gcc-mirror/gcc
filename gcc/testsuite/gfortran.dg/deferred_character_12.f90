! { dg-do run }
!
! Tests the fix for PR63232
!
! Contributed by Balint Aradi  <baradi09@gmail.com>
!
module mymod
  implicit none

  type :: wrapper
    character(:), allocatable :: string
  end type wrapper

contains


  subroutine sub2(mystring)
    character(:), allocatable, intent(out) :: mystring

    mystring = "test"

  end subroutine sub2

end module mymod


program test
  use mymod
  implicit none

  type(wrapper) :: mywrapper

  call sub2(mywrapper%string)
  if (.not. allocated(mywrapper%string)) STOP 1
  if (trim(mywrapper%string) .ne. "test") STOP 2

end program test
