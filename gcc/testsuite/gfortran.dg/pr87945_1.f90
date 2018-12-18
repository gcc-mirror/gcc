! { dg-do compile }
! PR fortran/87945
program p
   character :: a, b
   data a%len /1/       ! { dg-error "parameter cannot appear in" }
   data b%kind /'b'/    ! { dg-error "parameter cannot appear in" }
end
