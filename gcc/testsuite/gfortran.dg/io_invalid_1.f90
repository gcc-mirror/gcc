! { dg-do compile }
! PR fortran/20842
WRITE(UNIT=6,END=999) 0 ! { dg-error "END tag .* is not compatible with output" }
999 CONTINUE
END
