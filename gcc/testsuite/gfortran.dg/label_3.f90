! { dg-do compile }
! PR fortran/25756.
! This used to ICE due to the space after the label.
1 ! { dg-error "Statement label without statement" }
end
