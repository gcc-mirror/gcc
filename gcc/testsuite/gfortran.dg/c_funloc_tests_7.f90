! { dg-do compile }
! { dg-options "-std=f2008ts -fdump-tree-original" }
!
! Check relaxed TS29113 constraints for procedures
! and c_f_*pointer argument checking for c_ptr/c_funptr.
!

use iso_c_binding
implicit none
type(c_funptr) :: cfp

integer, external :: noCsub
procedure(integer), pointer :: fint

cfp = c_funloc (noCsub)
call c_f_procpointer (cfp, fint)
end

! { dg-final { scan-tree-dump-times "cfp =\[^;\]+ nocsub;" 1 "original" } }
! { dg-final { scan-tree-dump-times "fint =\[^;\]+ cfp;" 1 "original" } }

