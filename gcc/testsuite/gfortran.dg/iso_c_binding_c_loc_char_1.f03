! { dg-do compile }
! PR 38536 - don't reject substring of length one
! Original test case by Scot Breitenfeld
SUBROUTINE test(buf, buf2, buf3, n)
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(INOUT), TARGET :: buf
  INTEGER, INTENT(in) :: n
  CHARACTER(LEN=*), INTENT(INOUT), DIMENSION(1:2), TARGET :: buf2
  CHARACTER(LEN=3), TARGET :: buf3
  TYPE(C_PTR) :: f_ptr

  f_ptr = C_LOC(buf(1:1))      ! Used to fail
                               ! Error: CHARACTER argument 'buf' to 'c_loc'
                               ! at (1) must have a length of 1
  f_ptr = C_LOC(buf2(1)(1:1))  ! PASSES

  f_ptr = C_LOC(buf(n:n))

  f_ptr = C_LOC(buf3(3:))
END SUBROUTINE test
