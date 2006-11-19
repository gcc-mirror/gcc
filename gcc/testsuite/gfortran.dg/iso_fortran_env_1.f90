! { dg-do run }
module iso_fortran_env
  real :: x
end module iso_fortran_env

subroutine bar
  use , intrinsic :: iso_fortran_env
  implicit none

  if (file_storage_size /= 8) call abort
  if (character_storage_size /= 8) call abort
  if (all (numeric_storage_size /= [ 8, 16, 32, 64, 128])) call abort
  if (input_unit /= 5) call abort
  if (output_unit /= 6) call abort
  if (error_unit /= 0) call abort
  if (iostat_end /= -1) call abort
  if (iostat_eor /= -2) call abort
end

subroutine bar2
  use , intrinsic :: iso_fortran_env, only : file_storage_size, &
    character_storage_size, numeric_storage_size, input_unit, output_unit, &
    error_unit, iostat_end, iostat_eor
  implicit none

  if (file_storage_size /= 8) call abort
  if (character_storage_size /= 8) call abort
  if (all (numeric_storage_size /= [ 8, 16, 32, 64, 128])) call abort
  if (input_unit /= 5) call abort
  if (output_unit /= 6) call abort
  if (error_unit /= 0) call abort
  if (iostat_end /= -1) call abort
  if (iostat_eor /= -2) call abort
end

program test
  use , intrinsic :: iso_fortran_env, uu => output_unit
  implicit none

  if (input_unit /= 5 .or. uu /= 6) call abort
  call bar
  call bar2
end
! { dg-final { cleanup-modules "iso_fortran_env" } }
