! { dg-do compile }
! PR fortran/20842
WRITE(UNIT=6,END=999) 0 ! { dg-error "END tag .* not allowed in output statement" }
999 CONTINUE
END
