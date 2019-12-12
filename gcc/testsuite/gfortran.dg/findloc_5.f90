! { dg-do  run }
! Check compile-time simplification of FINDLOC
program main
  integer,  dimension(4),  parameter :: a1 = [1,  2,  3,  1]
  integer,  parameter :: i1 = findloc(a1, 1, dim=1)
  integer,  parameter :: i2 = findloc(a1, 2, dim=1)
  integer,  parameter :: i3 = findloc(a1, 3, dim=1)
  integer,  parameter :: i4 = findloc(a1, 1, dim=1, back=.true.)
  integer,  parameter :: i0 = findloc(a1, -1, dim=1)
  logical,  dimension(4),  parameter :: msk = [.false., .true., .true., .true.]
  integer,  parameter :: i4a = findloc(a1, 1, dim=1, mask=msk)
  integer,  parameter :: i4b = findloc(a1, 1, dim=1, mask=msk, back=.true.)
  real, dimension(2,2), parameter :: a = reshape([1.,2.,3.,4.], [2,2]), &
       b =  reshape([1.,2.,1.,2.], [2,2])
  integer, parameter, dimension(2) :: t8 = findloc(a, 5.), t9 = findloc(a, 5., back=.true.)
  integer, parameter, dimension(2) :: t10= findloc(a, 2.), t11= findloc(a, 2., back=.true.)
  logical, dimension(2,2), parameter :: lo = reshape([.true., .false., .true., .true. ], [2,2])
  integer, parameter, dimension(2) :: t12 = findloc(b,2., mask=lo)

  integer, dimension(2,3), parameter :: c = reshape([1,2,2,2,-9,6], [2,3])
  integer, parameter, dimension(3) :: t13 = findloc(c, value=2, dim=1)
  integer, parameter, dimension(2) :: t14 = findloc(c, value=2, dim=2)

  character(len=2), dimension(3,3), parameter :: ac = reshape ( &
       ["11", "21", "31", "12", "22", "32", "13", "23", "33"], [3,3]);
  character(len=3), dimension(3,3), parameter :: bc = reshape (&
       ["11 ", "21 ", "31 ", "12 ", "22 ", "32 ", "13 ", "23 ", "33 "], [3,3]);
  integer, parameter, dimension(2) :: t15 = findloc(ac, "11")
  integer, parameter, dimension(2) :: t16 = findloc(bc, "31")

  if (i1 /= 1) stop 1
  if (i2 /= 2) stop 2
  if (i3 /= 3) stop 3
  if (i4 /= 4) stop 4
  if (i0 /= 0) stop 5
  if (i4a /= 4) stop 6
  if (i4b /= 4) stop 7
  if (any(t8 /= [0,0])) stop 8
  if (any(t9 /= [0,0])) stop 9
  if (any(t10 /= [2,1])) stop 10
  if (any(t11 /= [2,1])) stop 11
  if (any(t12 /= [2,2])) stop 12
  if (any(t13 /= [2,1,0])) stop 13
  if (any(t14 /= [2,1])) stop 14
  if (any(t15 /= [1,1])) stop 15
  if (any(t16 /= [3,1])) stop 16
end program main
