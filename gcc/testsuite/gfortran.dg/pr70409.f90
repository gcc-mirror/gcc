! { dg-do run }
! PR fortran/70409
! Contriubted by Harald Anlauf  <anlauf at gmx dot de>
program foo
  integer, parameter :: huge_1 = huge(0_1)
  character(    huge_1      ), parameter :: x = 'abc'
  character(    huge(0_1)   ), parameter :: y = 'abc'
  character(    huge(0_1)+0 ), parameter :: z = 'abcdef'
  character(    huge(0_1)   )            :: a = 'abc'
  integer, parameter :: huge_2 = huge(0_2)
  character(    huge_2      ), parameter :: u = 'abc'
  character(    huge(0_2)   ), parameter :: v = 'abc'
  character(int(huge(0_2),4)), parameter :: w = 'abcdef'
  character(    huge(0_2)   )            :: b = 'abc'
  if (len(x) /= huge_1) stop 1
  if (len(y) /= huge_1) stop 2
  if (len(z) /= huge_1) stop 3
  if (len(a) /= huge_1) stop 4
  if (len(u) /= huge_2) stop 5
  if (len(v) /= huge_2) stop 6
  if (len(w) /= huge_2) stop 7
  if (len(b) /= huge_2) stop 8
end program foo
