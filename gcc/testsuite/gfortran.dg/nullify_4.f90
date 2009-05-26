! { dg-do compile }
! PR fortran/40246
!
! Check error recovery; was crashing before.
!
real, pointer :: ptr
nullify(ptr, mesh%coarser) ! { dg-error "Syntax error in NULLIFY statement" }
end
