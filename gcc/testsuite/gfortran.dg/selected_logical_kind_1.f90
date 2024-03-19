! { dg-do run }

program selected
  implicit none

  integer, parameter :: k = max(1, selected_logical_kind(128))
  logical(kind=k) :: l

  ! This makes assumptions about the targets, but they are true
  ! for all targets that gfortran supports

  if (selected_logical_kind(1)  /= 1) STOP 1
  if (selected_logical_kind(8)  /= 1) STOP 2
  if (selected_logical_kind(9)  /= 2) STOP 3
  if (selected_logical_kind(16) /= 2) STOP 4
  if (selected_logical_kind(17) /= 4) STOP 5
  if (selected_logical_kind(32) /= 4) STOP 6
  if (selected_logical_kind(33) /= 8) STOP 7
  if (selected_logical_kind(64) /= 8) STOP 8

  ! This should not exist

  if (selected_logical_kind(17921) /= -1) STOP 9

  ! We test for a kind larger than 64 bits separately

  if (storage_size(l) /= 8 * k) STOP 10

end program
