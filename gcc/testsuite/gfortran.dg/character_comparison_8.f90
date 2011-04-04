! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Check for compile-time optimization of LLE and friends.
program main
  character(3) :: a
  a = 'ab'
  if (.not. LLE(a,a)) call abort
  if (LLT(a,a)) call abort
  if (.not. LGE(a,a)) call abort
  if (LGT(a,a)) call abort
end program main
! { dg-final { scan-tree-dump-times "gfortran_compare_string" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

