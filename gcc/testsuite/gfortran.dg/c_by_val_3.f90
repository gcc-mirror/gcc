! { dg-do compile }
! { dg-options "-std=f95" }
program c_by_val_3
  external bar
  real (4) :: bar
  print *, bar (%VAL(0.0)) ! { dg-error "argument list function" }
end program c_by_val_3
