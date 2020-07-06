! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/95709 - ICE in gfc_resolve_code, at fortran/resolve.c:11807

program p
  integer, parameter :: i(1) = 1
  integer, parameter :: j    = 1
  integer            :: k(1) = 1
  goto i(1)        ! { dg-error "requires a scalar INTEGER variable" }
  goto j           ! { dg-error "requires a scalar INTEGER variable" }
  goto k(1)        ! { dg-error "requires a scalar INTEGER variable" }
  goto i%kind, (1) ! { dg-error "requires a scalar INTEGER variable" }
1 continue
end
