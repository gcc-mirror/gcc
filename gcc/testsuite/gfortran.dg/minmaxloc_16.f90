! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR fortran/104811
! Frontend-optimization mis-optimized minloc/maxloc of character arrays

program p
  character(1) :: str(3)
  str = ["a", "c", "a"]
  if (any (maxloc (str) /= 2)) stop 1
  if (minloc (str,dim=1) /= 1) stop 2
end

! { dg-final { scan-tree-dump-times "_gfortran_maxloc0_4_s1" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_minloc2_4_s1" 1 "original" } }
