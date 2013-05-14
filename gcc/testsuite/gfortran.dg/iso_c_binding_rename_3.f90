! { dg-do compile }
!
! PR fortran/55343
!
! Contributed by Janus Weil
!
module my_mod
  implicit none
  type int_type
    integer :: i
  end type int_type
end module my_mod
program main
  use iso_c_binding, only: C_void_ptr=>C_ptr, C_string_ptr=>C_ptr
  use my_mod, only: i1_type=>int_type, i2_type=>int_type
  implicit none
  type(C_string_ptr) :: p_string
  type(C_void_ptr) :: p_void
  type (i1_type) :: i1
  type (i2_type) :: i2
  p_void = p_string
  i1 = i2
end program main
