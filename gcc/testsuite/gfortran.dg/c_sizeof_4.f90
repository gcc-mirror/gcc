! { dg-do link }
!
! PR fortran/40568
!
! Module checks for C_SIZEOF (part of ISO_C_BINDING)
!

implicit none
intrinsic c_sizeof ! { dg-error "does not exist" }
end
