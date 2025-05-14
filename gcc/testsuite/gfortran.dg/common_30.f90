! { dg-do compile }
! PR fortran/119199
!
! One cannot SAVE an undefined COMMON block
!
! Contributed by David Binderman

program main
  save /argmnt1/ ! { dg-error "does not exist" }
end
