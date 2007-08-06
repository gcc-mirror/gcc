! { dg-do compile }
! { dg-options "-std=f95" }
  print *, min("foo", "bar") ! { dg-error "Fortran 2003.* CHARACTER argument" }
  end
