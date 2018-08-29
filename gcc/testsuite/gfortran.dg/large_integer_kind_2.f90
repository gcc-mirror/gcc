! { dg-do run }
! { dg-require-effective-target fortran_large_int }

! Testing library calls on large integer kinds (larger than kind=8)
  implicit none

  integer,parameter :: k = selected_int_kind (range (0_8) + 1)

  integer(kind=k) :: i, j
  integer(8) :: a, b

  i = 0; j = 1; a = i; b = j
  if (i ** j /= a ** b) STOP 1

end
