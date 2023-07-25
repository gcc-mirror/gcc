! PR fortran/99226

subroutine sub (n)
   integer :: n, i
   !$omp target	! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
   !$omp teams distribute dist_schedule (static,n+4)	! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
   do i = 1, 8
   end do
   !$omp teams distribute dist_schedule (static,n+4)
   do i = 1, 8
   end do
   !$omp end target
end
