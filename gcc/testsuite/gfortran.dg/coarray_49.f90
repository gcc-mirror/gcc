! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/101565 - ICE in gfc_simplify_image_index
! Contributed by G. Steinmetz

program p
  integer :: x[*]
  print *, image_index (x, [1.0]) ! { dg-error "shall be INTEGER" }
end
