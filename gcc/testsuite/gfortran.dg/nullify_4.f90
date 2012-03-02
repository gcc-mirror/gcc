! { dg-do compile }
! PR fortran/40246
!
! Check error recovery; was crashing before.
!
real, pointer :: ptr
nullify(ptr, mesh%coarser) ! { dg-error "Symbol 'mesh' at .1. has no IMPLICIT type" }
end
