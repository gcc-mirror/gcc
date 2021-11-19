! { dg-do compile }
!
module gbl_message
  type :: mytype
    integer(kind=4) :: e
  end type mytype
  type(mytype), parameter :: seve = mytype(1)
end module gbl_message

module gbl_interfaces
  interface
    subroutine gagout(message)
      character(len=*), intent(in) :: message
    end subroutine gagout
  end interface
end module gbl_interfaces

program test
  use gbl_message
  use gbl_interfaces
  call gagout(seve%e,'Some string') ! { dg-error "Type mismatch in argument|More actual than formal" }
end program test
