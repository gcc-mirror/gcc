! { dg-do compile }
! PR fortran/32555
!
2050  FORMAT(0PF9.4)
2050  FORMAT(0F9.4) ! { dg-error "Expected P edit descriptor" }
end
