! { dg-do compile }
! This used to ICE with an infinite recursion during development.
! Test case by Dominique d'Humieres.

program logtest3 
   implicit none 
   logical :: x = .true. 
   integer, parameter :: I_FINDLOC_BACK(1) = findloc([1,1],1, back=x) ! { dg-error "does not reduce to a constant expression" }
end program logtest3
