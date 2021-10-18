implicit none
include "nonexisting/file.f90"  ! { dg-error "Cannot open included file 'nonexisting/file.f90'" }
end
! { dg-prune-output "compilation terminated." }
