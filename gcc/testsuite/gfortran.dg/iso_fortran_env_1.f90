! { dg-do run }
module iso_fortran_env
  real :: x
end module iso_fortran_env

subroutine bar
  use , intrinsic :: iso_fortran_env
  implicit none

  if (file_storage_size /= 8) STOP 1
  if (character_storage_size /= 8) STOP 2
  if (all (numeric_storage_size /= [ 8, 16, 32, 64, 128])) STOP 3
  if (input_unit /= 5) STOP 4
  if (output_unit /= 6) STOP 5
  if (error_unit /= 0) STOP 6
  if (iostat_end /= -1) STOP 7
  if (iostat_eor /= -2) STOP 8
end

subroutine bar2
  use , intrinsic :: iso_fortran_env, only : file_storage_size, &
    character_storage_size, numeric_storage_size, input_unit, output_unit, &
    error_unit, iostat_end, iostat_eor
  implicit none

  if (file_storage_size /= 8) STOP 9
  if (character_storage_size /= 8) STOP 10
  if (all (numeric_storage_size /= [ 8, 16, 32, 64, 128])) STOP 11
  if (input_unit /= 5) STOP 12
  if (output_unit /= 6) STOP 13
  if (error_unit /= 0) STOP 14
  if (iostat_end /= -1) STOP 15
  if (iostat_eor /= -2) STOP 16
end

program test
  use , intrinsic :: iso_fortran_env, uu => output_unit
  implicit none

  if (input_unit /= 5 .or. uu /= 6) STOP 17
  call bar
  call bar2
end
