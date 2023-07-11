! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR fortran/110288 - FINDLOC and deferred-length character arguments

program test
  character(len=:), allocatable :: array(:)
  character(len=:), allocatable :: value
  array = ["bb", "aa"]
  value = "aa"
  if (findloc (array, value, dim=1) /= 2) stop 1
end program test

! { dg-final { scan-tree-dump "_gfortran_findloc2_s1 \\(.*, \\.array, \\.value\\)" "original" } }
