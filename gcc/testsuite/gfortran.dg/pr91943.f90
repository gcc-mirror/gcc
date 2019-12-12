! { dg-do compile }
! PR fortran/91943
! Code contributed by Gerhard Steinmetz
program p
   print *, f(b'1001')  ! { dg-error "cannot appear as an actual argument" }
   call sub(b'1001')    ! { dg-error "cannot appear as an actual argument" }
end
