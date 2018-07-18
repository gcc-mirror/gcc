! { dg-do run }
! { dg-options "-fdump-tree-original" }

MODULE mo_test

  integer :: n = 0
CONTAINS

  FUNCTION nquery()
    INTEGER :: nquery
    WRITE (0,*) "hello!"
    n = n + 1
    nquery = n
  END FUNCTION nquery

END MODULE mo_test


! ----------------------------------------------------------------------
! MAIN PROGRAM
! ----------------------------------------------------------------------
PROGRAM example
   USE mo_test
   INTEGER, ALLOCATABLE :: query_buf(:)
   ALLOCATE(query_buf(nquery()))
   if (n /= 1 .or. size(query_buf) /= n) STOP 1
END PROGRAM example

! { dg-final { scan-tree-dump-times "nquery" 5 "original" } }
