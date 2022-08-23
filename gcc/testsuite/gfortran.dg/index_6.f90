! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/105691 - Incorrect calculation of INDEX(str1,str2) at compile time

program main
  implicit none
  integer :: i
  character(*), parameter :: s1 = "fortran.f90"
  character(*), parameter :: s2 = "fortran"
  character(*), parameter :: s3 = s2 // "*"
  integer, parameter :: i0    = index(s1, s2)
  integer, parameter :: i1    = index(s1, s2, back= .true.)
  integer, parameter :: i2(*) = index(s1, s2, back=[.true.,.false.])
  integer, parameter :: i3(*) = index(s1, s2, back=[(i==1, i=1,2)] )
  integer, parameter :: i4    = index(s1, s3)
  integer, parameter :: i5    = index(s1, s3, back= .true.)
  integer, parameter :: i6(*) = index(s1, s3, back=[.true.,.false.])
  integer, parameter :: i7(*) = index(s1, s3, back=[(i==1, i=1,2)] )
  integer, parameter :: i8    = index(s1, "f", back= .true.)
  if (     i0 /= 1 ) stop 1
  if (     i1 /= 1 ) stop 2
  if (any (i2 /= 1)) stop 3
  if (any (i3 /= 1)) stop 4
  if (     i4 /= 0 ) stop 5
  if (     i5 /= 0 ) stop 6
  if (any (i6 /= 0)) stop 7
  if (any (i7 /= 0)) stop 8
  if (i8 /= len(s1)-2) stop 9
end program

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
