! { dg-do compile }
! PR fortran/95502 - ICE in gfc_check_do_variable, at fortran/parse.c:4446

program p
  integer, pointer :: z
  nullify (z%kind)  ! { dg-error "in variable definition context" }
  z%kind => NULL()  ! { dg-error "constant expression" }
end
