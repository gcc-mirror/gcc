! { dg-do run }
!
! Test the fix for PR98897 in which typebound subroutines of associate names
! were not recognised in a call. Functions were OK but this is tested below.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module output_data_m
  implicit none

  type output_data_t
    integer, private :: i = 0
  contains
    procedure output, return_value
  end type


contains
  subroutine output(self)
      implicit none
      class(output_data_t) self
      self%i = 1234
  end subroutine

  integer function return_value(self)
      implicit none
      class(output_data_t) self
      return_value = self%i
  end function
end module

  use output_data_m
  implicit none
  associate(output_data => output_data_t())
    call output_data%output
    if (output_data%return_value() .ne. 1234) stop 1
  end associate
end

