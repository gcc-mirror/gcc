! { dg-do compile }
! { dg-options "-finit-derived" }
!
! PR fortran/82886
!
! Test a regression which caused an ICE when -finit-derived was given without
! other -finit-* flags, especially for derived-type components with potentially
! hidden basic integer components.
!

program pr82886

  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
  type t
    type(c_ptr) :: my_c_ptr
  end type

contains

  subroutine sub0() bind(c)
    type(t), target :: my_f90_type
    my_f90_type%my_c_ptr = c_null_ptr
  end subroutine

end
