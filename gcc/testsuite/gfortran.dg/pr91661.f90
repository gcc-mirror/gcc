! { dg-do run }
! PR fortran/91661
! Contributed by Gerhard Steinmetz
! Verify that fix for PR92996 also fixes this one
program p
  integer, parameter :: a(2)    = 2
  integer, parameter :: b(a(1)) = 3
  integer, parameter :: c       = dot_product(b, b)
  integer, parameter :: d(a(1)+a(2)) = 3
  integer, parameter :: e = size (d,dim=1)
  if (c /= 18) stop 1   ! This used to ICE
  if (e /= 4)  stop 2   ! This used to ICE
end
