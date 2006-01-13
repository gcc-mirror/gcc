! { dg-do compile }
! PR fortran/25756.
! This used to ICE due to the space after the label.
1 ! { dg-warning "Ignoring statement label in empty statement" }
end
