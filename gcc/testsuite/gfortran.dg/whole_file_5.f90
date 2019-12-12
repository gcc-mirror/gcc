! { dg-do compile }
! { dg-options "-O3 -fdump-tree-optimized" }
! { dg-add-options bind_pic_locally }
!
! Check that inlining of functions declared BEFORE usage works.
! If yes, then the dump does not contain a call to F().
!

INTEGER FUNCTION f()
  f = 42
END FUNCTION

PROGRAM main
  INTEGER :: a, f
  a = f()
  print *, a, f()
END PROGRAM

! { dg-final { scan-tree-dump-times "= f \\(\\)" 0 "optimized" } }
