! { dg-do run }

module callback_box
  implicit none
  integer :: result = -1

  abstract interface
    subroutine action(value)
      integer, intent(in) :: value
    end subroutine action
  end interface
contains
  subroutine invoke(maybe_action)
    procedure(action), optional :: maybe_action

    call nested_invoke
  contains
    subroutine nested_invoke
      if (present(maybe_action)) call maybe_action(17)
    end subroutine nested_invoke
  end subroutine invoke

  subroutine record_value(value)
    integer, intent(in) :: value

    result = value
  end subroutine record_value
end module callback_box

program check_callback
  use callback_box, only : invoke, record_value, result
  implicit none

  call invoke
  if (result /= -1) error stop 1
  call invoke(record_value)
  if (result /= 17) error stop 2
end program check_callback
