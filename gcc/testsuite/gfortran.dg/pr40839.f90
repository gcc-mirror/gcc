! PR fortran/40839
! { dg-do compile }
write(fmt='(a)'), 'abc'         ! { dg-error "UNIT not specified" }
write(fmt='()')                 ! { dg-error "UNIT not specified" }
end
