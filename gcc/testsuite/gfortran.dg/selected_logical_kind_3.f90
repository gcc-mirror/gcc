! { dg-do run }
! { dg-require-effective-target fortran_integer_16 }

program selected
  implicit none

  integer, parameter :: k1 = selected_logical_kind(128)
  logical(kind=k1) :: l

  integer, parameter :: k2 = selected_int_kind(25)
  integer(kind=k2) :: i

  if (storage_size(l) /= 8 * k1) STOP 1
  if (storage_size(i) /= 8 * k2) STOP 2
  if (bit_size(i) /= 8 * k2) STOP 3
  if (k1 /= k2) STOP 4

end program
