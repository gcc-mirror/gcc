! { dg-do compile }
! PR fortran/50556
subroutine foo
   save i
   namelist /i/ ii    ! { dg-error "cannot have the SAVE attribute" }
end subroutine foo
subroutine bar
   namelist /i/ ii
   save i             ! { dg-error "cannot have the SAVE attribute" }
end subroutine bar
