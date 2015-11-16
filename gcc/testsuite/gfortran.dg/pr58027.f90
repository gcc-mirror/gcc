! { dg-do compile }
! PR fortran/58027
integer, parameter :: i(1)=(/z'ff800000'/) ! { dg-error "overflow converting" }
print *, isclass
end
