! { dg-do compile }
! This should compile.  There was a bug in resolving c_f_pointer that was 
! caused by not sorting the actual args to match the order of the formal args.
! PR fortran/32800
!
FUNCTION C_F_STRING(CPTR) RESULT(FPTR)
  USE ISO_C_BINDING
  implicit none
  TYPE(C_PTR), INTENT(IN) :: CPTR ! The C address
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: FPTR
  INTERFACE
     FUNCTION strlen(string) RESULT(len) BIND(C,NAME="strlen")
       import
       TYPE(C_PTR), VALUE :: string ! A C pointer
       integer(c_int) :: len
     END FUNCTION strlen
  END INTERFACE
  CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR,SHAPE=[strlen(cptr)])
END FUNCTION C_F_STRING

