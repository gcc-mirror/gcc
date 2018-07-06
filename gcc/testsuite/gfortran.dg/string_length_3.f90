! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 78021 - calls to mylen were folded after shortening the
! argument list.

PROGRAM test_o_char
  implicit none
  integer :: n
  n = mylen('c') + mylen('c   ')
  if (n /= 5) STOP 1
CONTAINS

  FUNCTION mylen(c)
    CHARACTER(len=*),INTENT(in) :: c
    INTEGER :: mylen
    mylen=LEN(c)
  END FUNCTION mylen
END PROGRAM test_o_char
! { dg-final { scan-tree-dump-times "__var" 0 "original" } }
