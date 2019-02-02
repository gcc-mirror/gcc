! { dg-do preprocess }
! { dg-additional-options "-cpp" }
! PR 57048 - this used not to compile. Original test case by Angelo
! Graziosi.  Only works if compiled c_funptr_1_mod.f90, hence the
! do-nothing directive above.
module procs
  
  implicit none
  private

  public WndProc

contains
  function WndProc()
    integer :: WndProc
    
    WndProc = 0
  end function WndProc
end module procs

function WinMain()
  use, intrinsic :: iso_c_binding, only: C_INT,c_sizeof,c_funloc
  use win32_types
  use procs
  implicit none

  integer :: WinMain

  type(WNDCLASSEX_T) :: WndClass

  WndClass%cbSize = int(c_sizeof(Wndclass),C_INT)
  WndClass%lpfnWndProc = c_funloc(WndProc)

  WinMain = 0
end function WinMain

program main
end 
