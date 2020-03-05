! { dg-do compile }
! PR fortran/92898
! Code contributed by Gerhard Steinmetz
program p
  print *, is_contiguous (null())     ! { dg-error "shall be an associated" }
end
