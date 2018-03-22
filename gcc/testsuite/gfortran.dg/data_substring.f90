! { dg-do compile }
! PR fortran/30792
character string*1025
integer i
data (string(i:i),i=1,1025)/1025*'?'/  ! { dg-error "Invalid substring" }
end
