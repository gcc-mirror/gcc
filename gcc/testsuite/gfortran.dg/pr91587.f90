! { dg-do compile }
! PR fortran/91587
! Code contributed by Gerhard Steinmetz
program p
   backspace(err=!)  ! { dg-error "Syntax error in" }
   flush(err=!)      ! { dg-error "Syntax error in" }
   rewind(err=!)     ! { dg-error "Syntax error in" }
end

subroutine bar       ! An other matcher runs, and gives a different error.
   endfile(err=!)    ! { dg-error "Expecting END" }
end
