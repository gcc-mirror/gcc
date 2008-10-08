! { dg-do compile }
! { dg-options "-fdump-tree-original" }

! PR fortran/36403
! Check that string lengths of optional arguments are added to the library-call
! even if those arguments are missing.

PROGRAM main
  IMPLICIT NONE

  CHARACTER(len=1) :: vect(4)
  CHARACTER(len=1) :: matrix(2, 2)

  matrix(1, 1) = ""
  matrix(2, 1) = "a"
  matrix(1, 2) = "b"
  matrix(2, 2) = ""
  vect = (/ "w", "x", "y", "z" /)

  ! Call the affected intrinsics
  vect = EOSHIFT (vect, 2)
  vect = PACK (matrix, matrix /= "")
  matrix = RESHAPE (vect, (/ 2, 2 /))

END PROGRAM main

! All library function should be called with *two* trailing arguments "1" for
! the string lengths of both the main array and the optional argument:
! { dg-final { scan-tree-dump "_eoshift\[0-9_\]+char \\(\[&a-zA-Z0-9._, \]+, 1, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_reshape\[0-9_\]+char \\(\[&a-zA-Z0-9._, \]+, 1, 0\\)" "original" } }
! { dg-final { scan-tree-dump "_pack\[0-9_\]+char \\(\[&a-zA-Z0-9._, \]+, 1, 0\\)" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
