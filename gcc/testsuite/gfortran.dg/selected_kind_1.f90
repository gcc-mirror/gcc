! { dg-do run }
! { dg-options "-fdefault-integer-8" }
! PR fortran/32968
program selected

  if (selected_int_kind (1)  /= 1) STOP 1
  if (selected_int_kind (3)  /= 2) STOP 2
  if (selected_int_kind (5)  /= 4) STOP 3
  if (selected_int_kind (10) /= 8) STOP 4
  if (selected_real_kind (1)  /= 4) STOP 5
  if (selected_real_kind (2)  /= 4) STOP 6
  if (selected_real_kind (9)  /= 8) STOP 7
  if (selected_real_kind (10) /= 8) STOP 8

end program selected

