! { dg-do  run }
! { dg-additional-options "-fdump-tree-original" }
program main
  implicit none
  integer, parameter :: z(0) = 0
  integer, parameter, dimension(1) :: a = minloc(z)
  integer, parameter, dimension(1) :: b = minloc(z,mask=z>0)
  integer, parameter :: c = minloc(z,dim=1)

  integer, parameter, dimension(1) :: d = maxloc(z)
  integer, parameter, dimension(1) :: e = maxloc(z,mask=z>0)
  integer, parameter :: f = maxloc(z,dim=1)

  character(len=12) line

  if (a(1) /= 0) stop 1
  if (b(1) /= 0) stop 2
  if (c /= 0) stop 3

  if (d(1) /= 0) stop 4
  if (e(1) /= 0) stop 5
  if (f /= 0) stop 6

  write (unit=line,fmt='(6I2)') minloc(z), minloc(z,mask=z>0), minloc(z,dim=1), &
       maxloc(z), maxloc(z,mask=z<0), maxloc(z,dim=1)
  if (line /= ' 0 0 0 0 0 0') stop 7
end program main
! { dg-final { scan-tree-dump-times "_gfortran_stop" 1 "original" } }
