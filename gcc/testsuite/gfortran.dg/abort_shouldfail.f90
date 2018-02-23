! { dg-do  run }
! { dg-shouldfail "Program aborted." }
program main
  call abort
end program main
