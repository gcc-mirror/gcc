! { dg-do  run }
! { dg-options -std=gnu }
! { dg-additional-sources altreturn_9_1.f90 }
! PR 89496 - wrong type for alternate return was generated

program main
  call sub(10, *10, 20)
  stop 1
10 continue
end program main
