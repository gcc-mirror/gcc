! { dg-do run }
! { dg-options "-funsigned" }
program memain
  if (range(1u_1) /= 2) error stop 1
  if (range(1u_2) /= 4) error stop 2
  if (range(1u_4) /= 9) error stop 3
  if (range(1u_8) /= 19) error stop 4
end program memain
