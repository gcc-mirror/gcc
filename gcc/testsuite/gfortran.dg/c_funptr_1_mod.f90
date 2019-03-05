! { dg-do  run }
! { dg-additional-sources c_funptr_1.f90 }
! Additional module to go with c_funptr_1.f90
module win32_types
  use, intrinsic :: iso_c_binding, only: C_INT,C_FUNPTR
  implicit none
  private

  public WNDCLASSEX_T
  type, bind(C) :: WNDCLASSEX_T
     integer(C_INT) :: cbSize
     type(C_FUNPTR) :: lpfnWndProc

  end type WNDCLASSEX_T

end module win32_types
