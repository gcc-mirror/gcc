! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics -fdump-tree-original" }

! More sophisticated BLOCK runtime checks for correct initialization/clean-up.

PROGRAM main
  IMPLICIT NONE
  INTEGER :: n

  n = 5

  myblock: BLOCK
    INTEGER :: arr(n)
    IF (SIZE (arr) /= 5) CALL abort ()
    BLOCK
      INTEGER :: arr(2*n)
      IF (SIZE (arr) /= 10) CALL abort ()
    END BLOCK
    IF (SIZE (arr) /= 5) CALL abort ()
  END BLOCK myblock

  BLOCK
    INTEGER, ALLOCATABLE :: alloc_arr(:)
    IF (ALLOCATED (alloc_arr)) CALL abort ()
    ALLOCATE (alloc_arr(n))
    IF (SIZE (alloc_arr) /= 5) CALL abort ()
    ! Should be free'ed here (but at least somewhere), this is checked
    ! with pattern below.
  END BLOCK

  BLOCK
    CHARACTER(LEN=n) :: str
    IF (LEN (str) /= 5) CALL abort ()
    str = "123456789"
    IF (str /= "12345") CALL abort ()
  END BLOCK
END PROGRAM main
! { dg-final { scan-tree-dump-times "free \\(\\(void \\*\\) alloc_arr\\.data" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
