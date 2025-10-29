! { dg-do run }
!
! Test fix for PR122433
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module neuron_m
  implicit none

  type string_t
    character(len=:), allocatable :: string_
  end type

  type neuron_t(k)
    integer, kind :: k = kind(1.)
    real(k) bias_
    type(neuron_t(k)), allocatable :: next
  end type

contains
  recursive function from_json(neuron_lines, start) result(neuron)
    type(string_t) neuron_lines(:)
    integer start
    type(neuron_t) neuron
    character(len=:), allocatable :: line
    line = neuron_lines(start+1)%string_
    read(line(index(line, ":")+1:), fmt=*) neuron%bias_
    line = adjustr(neuron_lines(start+3)%string_)
! Used to give "Error: Syntax error in IF-clause" for next line.
    if (line(len(line):) == ",") neuron%next = from_json(neuron_lines, start+4)
  end function
  recursive function from_json_8(neuron_lines, start) result(neuron)
    type(string_t) neuron_lines(:)
    integer start
    type(neuron_t(kind(1d0))) neuron
    character(len=:), allocatable :: line
    line = neuron_lines(start+1)%string_
    read(line(index(line, ":")+1:), fmt=*) neuron%bias_
    line = adjustr(neuron_lines(start+3)%string_)
    if (line(len(line):) == ",") neuron%next = from_json_8(neuron_lines, start+4)
  end function
end module

  use neuron_m
  call foo
  call bar
contains
  subroutine foo
    type(neuron_t) neuron
    type(string_t) :: neuron_lines(8)
    neuron_lines(2)%string_ = "real : 4.0 "
    neuron_lines(4)%string_ = " ,"
    neuron_lines(6)%string_ = "real : 8.0 "
    neuron_lines(8)%string_ = " "
    neuron = from_json(neuron_lines, 1)
    if (int (neuron%bias_) /= 4) stop 1
    if (allocated (neuron%next)) then
      if (int (neuron%next%bias_) /= 8) stop 2
    else
      stop 3
    endif
  end subroutine
  subroutine bar
    type(neuron_t(kind(1d0))) neuron
    type(string_t) :: neuron_lines(8)
    neuron_lines(2)%string_ = "real : 4.0d0 "
    neuron_lines(4)%string_ = " ,"
    neuron_lines(6)%string_ = "real : 8.0d0 "
    neuron_lines(8)%string_ = " "
    neuron = from_json_8(neuron_lines, 1)
    if (int (neuron%bias_) /= 4) stop 1
    if (allocated (neuron%next)) then
      if (int (neuron%next%bias_) /= 8) stop 2
    else
      stop 3
    endif
  end subroutine
end
