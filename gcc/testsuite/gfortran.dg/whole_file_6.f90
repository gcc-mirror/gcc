! { dg-do compile }
! { dg-options "-O3 -fwhole-file -fdump-tree-optimized" }
! { dg-add-options bind_pic_locally }
!
! Check that inlining of functions declared AFTER usage works.
! If yes, then the dump does not contain a call to F().
!

PROGRAM main
  INTEGER :: a(3), f
  a = f()
  print *, a
END PROGRAM

INTEGER FUNCTION f()
  f = 42
END FUNCTION

! { dg-final { scan-tree-dump-times "= f \\(\\)" 0 "optimized" } }
