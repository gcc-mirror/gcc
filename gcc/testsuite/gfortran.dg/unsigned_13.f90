! { dg-do run }
! { dg-options "-funsigned" }
! Test basic functionality of ishft and ishftc.
program main
  unsigned :: u_a
  u_a = 1u
  if (ishft(1u,31) /= 2147483648u) error stop 1
  if (ishft(u_a,31) /= 2147483648u) error stop 2

  u_a = 3u
  if (ishft(3u,2) /= 12u) error stop 3
  if (ishft(u_a,2) /= 12u) error stop 4

  u_a = huge(u_a)
  if (ishftc(huge(u_a),1) /= huge(u_a)) error stop 5
  if (ishftc(u_a,1) /= u_a) error stop 6

end program
