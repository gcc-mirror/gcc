! { dg-do compile }
! PR fortran/91587
! Code contributed by Gerhard Steinmetz
program p
   backspace(err=!)  ! { dg-error "Invalid value for" }
   flush(err=!)      ! { dg-error "Invalid value for" }
   rewind(err=!)     ! { dg-error "Invalid value for" }
end

subroutine bar       ! An other matcher runs, and gives a different error.
   endfile(err=!)    ! { dg-error "Expecting END" }
end
