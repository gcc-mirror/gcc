! { dg-do compile }
! PR fortran/103715 - ICE in gfc_find_gsymbol
!
! valgrind did report an invalid read in check_externals_procedure

program p
  select type (y => g()) ! { dg-error "Selector shall be polymorphic" }
  end select
  call g()
end

! { dg-prune-output "already being used as a FUNCTION" }
