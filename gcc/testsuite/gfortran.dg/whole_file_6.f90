! { dg-do "compile" }
! { dg-options "-O3 -fwhole-file -fdump-tree-optimized" }
!
! Check that inlining of functions declared AFTER usage works.
! If yes, then the dump does not contain a call to F().
!

PROGRAM main
  INTEGER :: a(3)
  a = f()
  print *, a
END PROGRAM

INTEGER FUNCTION f()
  f = 42.0
END FUNCTION

! { dg-final { scan-tree-dump-times "= f\(\)" 0 "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
