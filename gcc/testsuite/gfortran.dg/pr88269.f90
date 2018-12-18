! { dg-do compile }
! PR fortran/88269
program p
   write (end=1e1) ! { dg-error "tag not allowed" }
end

