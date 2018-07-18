! { dg-do compile }
! { dg-options "-finit-derived -finit-local-zero -fdump-tree-original" }
!
! PR fortran/82972
!
! Make sure we do not ICE when generating initializers for c_ptr and c_funptr
! components of derived types (and make sure they are properly initialized to
! zero).
!

program init_flag_17
  use iso_c_binding
  implicit none

  type :: ty
    type(c_ptr)    :: ptr  ! = c_null_ptr
    type(c_funptr) :: fptr ! = c_null_funptr
  end type

  type(ty) :: t

  print *, t%ptr
  print *, t%fptr

end program

! { dg-final { scan-tree-dump "\.ptr=0" "original" } }
! { dg-final { scan-tree-dump "\.fptr=0" "original" } }
