! { dg-do compile }
! { dg-options "-Wundefined-vars" }
! PR fortran/126558 - this used to give a false positive for y.

program memain
  implicit none
  integer :: x,y
  associate (ax => x)
  end associate
  print *, x ! { dg-warning "Undefined variable" }
  associate (ay => y)
    ay = 42
  end associate
  print *,y
end program memain
