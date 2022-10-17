implicit none
include "."  ! { dg-error "Included file '.' is not a regular file" }
end
! { dg-prune-output "compilation terminated." }
