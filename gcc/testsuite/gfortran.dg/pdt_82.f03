! { dg-do run }
!
! Test the fix for pr123545, which caused the errors below. Although some of thses errors are
! not checked here, it has been verified that they are fixed by the patch for the main fault.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module julienne_m
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  type file_t
    type(string_t), allocatable :: lines_(:)
  end type

  interface file_t               ! If this generic interface was removed, a
    module procedure from_lines  ! segmentation fault resulted during or just after
  end interface                  ! the first executable statement in the main program.

contains

  function get_json_value(self ) result(value_)
    type(string_t), intent(in) :: self
    real value_
    read(self%string_, fmt=*) value_
!    print *," value_ ", value_
  end function

  pure function from_lines(lines) result(file_object)
    type(string_t), intent(in) :: lines(:)
    type(file_t) file_object
    file_object%lines_ = lines
  end function

end module

module fiats_m
  use julienne_m
  implicit none

  type hyperparameters_t(k)
    integer, kind :: k = kind(1.)
    real(k) :: learning_rate_ = real(1.5,k)
  end type

  interface hyperparameters_t
    module procedure hyperparameters_from_json
  end interface

  type, extends(file_t) :: training_configuration_t(m)
    integer, kind :: m = kind(1.)
    type(hyperparameters_t(m)) hyperparameters_
  end type

contains

  function hyperparameters_from_json(lines) result(hyperparameters)
    type(string_t), intent(in) :: lines(:)
    type(hyperparameters_t) hyperparameters
    hyperparameters%learning_rate_ = get_json_value(lines(1))
  end function

  pure function hyperparameters_to_json(self) result(lines)
    type(hyperparameters_t), intent(in) :: self
    type(string_t), allocatable :: lines(:)
    integer, parameter :: max_width= 18
    character(len=max_width) learning_rate_string
    write(learning_rate_string,*) self%learning_rate_
    lines = [string_t(learning_rate_string)]
  end function

  pure function training_configuration_from_components(hyperparameters) result(training_configuration)
    type(hyperparameters_t), intent(in) :: hyperparameters
    type(training_configuration_t) training_configuration
    training_configuration%hyperparameters_ = hyperparameters
    training_configuration%file_t = file_t([hyperparameters_to_json(training_configuration%hyperparameters_)])
  end function

  function training_configuration_from_file(line) result(training_configuration)
    character(len=*), intent(in) :: line
    type(training_configuration_t) training_configuration
    training_configuration%file_t = file_t([string_t(line)])
    training_configuration%hyperparameters_ = hyperparameters_from_json(training_configuration%file_t%lines_)
  end function

end module

  use fiats_m
  implicit none

  call test

contains

  subroutine test
    type(training_configuration_t) training_configuration, from_json

    training_configuration = training_configuration_from_components(hyperparameters_t(learning_rate_=1.))

    ! Removing the above assignment eliminated the segmentation fault even though the segmentation fault
    ! occured when executing the assignment below, which does not reference the object defined above.
    ! Alternatively, changing the above line to an `associate` statement gave the compile-time
    ! message: "Error: Invalid kind for REAL at (1)", where the "1" is between `use` and `fiats_m` in
    ! the above use statement.

    from_json = training_configuration_from_file('1.00000000')
    if (int (1d6 * from_json%hyperparameters_%learning_rate_) /= 1000000) stop 1
  end
end