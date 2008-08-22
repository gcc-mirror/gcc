! { dg-do run }
! { dg-options "-fdump-tree-original" }

! PR fortran/36403
! Check that the string length of BOUNDARY is added to the library-eoshift
! call even if BOUNDARY is missing (as it is optional).
! This is the original test from the PR.

! Contributed by Kazumoto Kojima.

  CHARACTER(LEN=3), DIMENSION(10) :: Z
  call test_eoshift
contains
  subroutine test_eoshift 
    CHARACTER(LEN=1), DIMENSION(10) :: chk
    chk(1:8) = "5"
    chk(9:10) = " "
    Z(:)="456"
    if (any (EOSHIFT(Z(:)(2:2),2) .ne. chk)) call abort 
  END subroutine
END

! Check that _gfortran_eoshift* is called with 8 arguments:
! { dg-final { scan-tree-dump "_gfortran_eoshift\[0-9_\]+char \\(\[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*, \[&a-zA-Z0-9._\]*\\)" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
