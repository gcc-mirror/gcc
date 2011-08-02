! { dg-do compile }
! Tests the fix for PR36526, in which the call to getStrLen would
! generate an error due to the use of a wrong symbol in interface.c
!
! Contributed by Bálint Aradi <aradi@bccms.uni-bremen.de>
!
module TestPure
  implicit none

  type T1
    character(10) :: str
  end type T1

contains

  pure function getT1Len(self) result(t1len)
    type(T1), pointer :: self
    integer :: t1len

    t1len = getStrLen(self%str)

  end function getT1Len


  pure function getStrLen(str) result(length)
    character(*), intent(in) :: str
    integer :: length

    length = len_trim(str)

  end function getStrLen

end module TestPure


program Test
  use TestPure
  implicit none

  type(T1), pointer :: pT1

  allocate(pT1)
  pT1%str = "test"
  write (*,*) getT1Len(pT1)
  deallocate(pT1)

end program Test
! { dg-final { cleanup-modules "testpure" } }
