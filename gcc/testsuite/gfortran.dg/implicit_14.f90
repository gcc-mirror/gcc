! { dg-do compile }
! { dg-options "-std=f2008ts" }
!
! Support Fortran 2015's IMPLICIT NONE with spec list
! (currently implemented as vendor extension)

implicit none (type) ! { dg-error "GNU Extension: IMPORT NONE with spec list at \\(1\\)" }
end
