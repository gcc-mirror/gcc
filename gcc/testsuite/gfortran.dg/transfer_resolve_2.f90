! { dg-do compile }
!
! PR fortran/56079
!
! Contributed by  Thomas Koenig
!
program gar_nichts
   use ISO_C_BINDING
   use ISO_C_BINDING, only: C_PTR
   use ISO_C_BINDING, only: abc => C_PTR
   use ISO_C_BINDING, only: xyz => C_PTR
   type(xyz) nada
   nada = transfer(C_NULL_PTR,nada)
end program gar_nichts
