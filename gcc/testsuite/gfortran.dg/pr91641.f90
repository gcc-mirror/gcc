! { dg-do compile }
! PR fortran/91641
! Code conyributed by Gerhard Steinmetz
program p
   real, pointer :: z(:)
   print *, is_contiguous (null(z))    ! { dg-error "shall be an associated" }
end
