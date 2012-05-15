! { dg-do compile }
! { dg-options "-Wc-binding-type" }
! This should compile, though there is a warning about the type of len
! (return variable of strlen()) for being implicit.
! PR fortran/32797
!
MODULE ISO_C_UTILITIES
   USE ISO_C_BINDING
   implicit none
   CHARACTER(C_CHAR), DIMENSION(1), SAVE, TARGET, PRIVATE :: dummy_string="?"
CONTAINS
   FUNCTION C_F_STRING(CPTR) RESULT(FPTR)
     use, intrinsic :: iso_c_binding
      TYPE(C_PTR), INTENT(IN) :: CPTR ! The C address
      CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: FPTR
      INTERFACE
         FUNCTION strlen(string) RESULT(len) BIND(C,NAME="strlen") ! { dg-warning "Implicitly declared" }
            USE ISO_C_BINDING
            TYPE(C_PTR), VALUE :: string ! A C pointer
         END FUNCTION
      END INTERFACE
      CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR, SHAPE=[strlen(CPTR)])
   END FUNCTION
END MODULE ISO_C_UTILITIES
