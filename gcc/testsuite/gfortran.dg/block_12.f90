! { dg-do compile }
! PR 50627 - this used to free a namespace twice.
program main
  block
end program main ! { dg-error "END BLOCK" }
! { dg-prune-output "Unexpected end of file" }
