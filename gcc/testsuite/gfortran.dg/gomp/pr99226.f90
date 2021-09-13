! PR fortran/99226

subroutine sub (n)
   integer :: n, i
   !$omp target	! { dg-error "construct with nested 'teams' construct contains directives outside of the 'teams' construct" }
   !$omp teams distribute dist_schedule (static,n+4)
   do i = 1, 8
   end do
   !$omp teams distribute dist_schedule (static,n+4)
   do i = 1, 8
   end do
   !$omp end target
end
