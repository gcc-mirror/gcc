! { dg-do compile }
! { dg-options "-Wno-c-binding-type" }
!
! That's a copy of "bind_c_usage_8.f03", "bind_c_dts_4.f03",
! "bind_c_implicit_vars.f03" and "c_kind_tests_2.f03"
! to check that with -Wno-c-binding-type no warning is printed.
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
         FUNCTION strlen(string) RESULT(len) BIND(C,NAME="strlen")
            USE ISO_C_BINDING
            TYPE(C_PTR), VALUE :: string ! A C pointer
         END FUNCTION
      END INTERFACE
      CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR, SHAPE=[strlen(CPTR)])
   END FUNCTION
END MODULE ISO_C_UTILITIES

module test
use iso_c_binding, only: c_int
    type, bind(c) ::  foo
      integer :: p
    end type
    type(foo), bind(c) :: cp
end module test

module bind_c_implicit_vars

bind(c) :: j

contains
  subroutine sub0(i) bind(c)
    i = 0
  end subroutine sub0
end module bind_c_implicit_vars

module c_kind_tests_2
  use, intrinsic :: iso_c_binding

  integer, parameter :: myF = c_float
  real(myF), bind(c) :: myCFloat
  integer(myF), bind(c) :: myCInt       ! { dg-warning "is for type REAL" }
  integer(c_double), bind(c) :: myCInt2 ! { dg-warning "is for type REAL" }

  integer, parameter :: myI = c_int
  real(myI) :: myReal             ! { dg-warning "is for type INTEGER" }
  real(myI), bind(c) :: myCFloat2 ! { dg-warning "is for type INTEGER" }
  real(4), bind(c) :: myFloat
end module c_kind_tests_2

! { dg-final { cleanup-modules "c_kind_tests_2" } }
! { dg-final { cleanup-modules "bind_c_implicit_vars" } }
! { dg-final { cleanup-modules "test" } }
! { dg-final { cleanup-modules "iso_c_utilities" } }
