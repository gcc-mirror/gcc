! { dg-do compile }
! PR fortran/95500 - ICE compiling extra interface on intrinsic

program test_intrinsic
  implicit none
  intrinsic :: alog
  intrinsic :: dlog
  real (4), parameter :: one = 1

  interface ln
     procedure :: alog, dlog
  end interface ln

  write (*,*) 'ln  1', ln (one)
end program test_intrinsic
