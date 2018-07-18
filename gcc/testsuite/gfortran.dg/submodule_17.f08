! { dg-do run }
!
! Tests the fix for PR77358, in which the wrong gfc_charlen was
! being used for the result of 'get'.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module hello_interface
  character(len=13) :: string="Hello, world!"
  interface
    module function get() result(result_string)
      character(:), allocatable :: result_string
    end function
  end interface
end module

submodule(hello_interface) hello_implementation
contains
  module function get() result(result_string)
    character(:), allocatable :: result_string
    result_string = string
  end function
end submodule

  use hello_interface
  if (get() .ne. string) STOP 1
end
