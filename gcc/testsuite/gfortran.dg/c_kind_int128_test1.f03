! { dg-do compile }
! { dg-options "-std=f2003" }
! { dg-require-effective-target fortran_integer_16 }
!

subroutine c_kind_int128_1
  use, intrinsic :: iso_c_binding
  implicit none

  integer(c_int128_t) :: a   ! { dg-error "has no IMPLICIT type" }
  integer(c_int_least128_t) :: b   ! { dg-error "has no IMPLICIT type" }
  integer(c_int_fast128_t) :: c   ! { dg-error "has no IMPLICIT type" }

end subroutine c_kind_int128_1


subroutine c_kind_int128_2
  use, intrinsic :: iso_c_binding

  integer(c_int128_t) :: a   ! { dg-error "has not been declared or is a variable" }
  integer(c_int_least128_t) :: b   ! { dg-error "has not been declared or is a variable" }
  integer(c_int_fast128_t) :: c   ! { dg-error "has not been declared or is a variable" }

end subroutine c_kind_int128_2
