! { dg-do run }
! PR fortran/109186 - Verify that NEAREST produces same results at
!                     compile-time and run-time for corner cases
! Reported by John Harper

program p
  implicit none
  integer,  parameter :: sp = selected_real_kind (6)
  integer,  parameter :: dp = selected_real_kind (13)
  real(sp), parameter :: x1 = huge (1._sp), t1 = tiny (1._sp)
  real(dp), parameter :: x2 = huge (1._dp), t2 = tiny (1._dp)
  real(sp), volatile  :: y1, z1
  real(dp), volatile  :: y2, z2
  y1 = x1
  z1 = nearest (y1, -1._sp)
  if (nearest (x1,  -1._sp) /= z1) stop 1
  y2 = x2
  z2 = nearest (y2, -1._dp)
  if (nearest (x2,  -1._dp) /= z2) stop 2
  y1 = t1
  z1 = nearest (y1,  1._sp)
  if (nearest (t1,   1._sp) /= z1) stop 3
  y2 = t2
  z2 = nearest (y2,  1._dp)
  if (nearest (t2,   1._dp) /= z2) stop 4
end
