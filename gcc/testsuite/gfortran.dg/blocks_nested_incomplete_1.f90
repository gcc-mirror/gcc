! { dg-do compile }
!
! PR fortran/66175
! Nested incomplete blocks cause ICE.
program main
  block
    block
end program ! { dg-error "Expecting END BLOCK statement" }
! { dg-prune-output "Unexpected end of file" }
