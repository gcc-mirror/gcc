! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/110585 - simplification of FINDLOC for constant complex arguments

program mvce
  implicit none
  integer, parameter :: a(*) = findloc([(1.,0.),(2.,1.)], (2.,0.))
  integer, parameter :: b(*) = findloc([(1.,0.),(2.,1.)], (2.,0.), back=.true.)
  integer, parameter :: c(*) = findloc([(1.,0.),(2.,1.)], (2.,1.))
  integer, parameter :: d(*) = findloc([(1.,0.),(2.,1.)], (2.,1.), back=.true.)
  integer, parameter :: e    = findloc([(1.,0.),(2.,1.)], (2.,1.), dim=1)
  if (a(1) /= 0) stop 1
  if (b(1) /= 0) stop 2
  if (c(1) /= 2) stop 3
  if (d(1) /= 2) stop 4
  if (e    /= 2) stop 5
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
