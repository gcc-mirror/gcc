! { dg-do "compile" }
! { dg-options "-O3 -fwhole-file -fdump-tree-optimized" }
!
! Check that inlining of functions declared BEFORE usage works.
! If yes, then the dump does not contain a call to F().
!

INTEGER FUNCTION f()
  f = 42
END FUNCTION

PROGRAM main
  INTEGER :: a
  a = f()
  print *, a
END PROGRAM

! { dg-final { scan-tree-dump-times "= f\(\)" 0 "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
