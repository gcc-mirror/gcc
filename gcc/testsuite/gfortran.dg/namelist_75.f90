! { dg-do compile }
!
! Tests a write-after-free memory error fix in gfc_undo_symbols

program test_nml

  namelist /foo/ bar, baz
  namelist /foo/ wrong, ,  ! { dg-error "Syntax error in NAMELIST" }

end program test_nml
