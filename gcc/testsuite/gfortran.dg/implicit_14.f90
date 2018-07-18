! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Support Fortran 2018's IMPLICIT NONE with spec list
! (currently implemented as vendor extension)

implicit none (type) ! { dg-error "Fortran 2018: IMPORT NONE with spec list at \\(1\\)" }
end
