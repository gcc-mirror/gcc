! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Check for compile-time optimization of LLE and friends.
program main
  character(3) :: a
  a = 'ab'
  if (.not. LLE(a,a)) STOP 1
  if (LLT(a,a)) STOP 2
  if (.not. LGE(a,a)) STOP 3
  if (LGT(a,a)) STOP 4
end program main
! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }

