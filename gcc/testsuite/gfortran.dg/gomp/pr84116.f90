! PR fortran/84116
! { dg-do compile }

program pr84116
   integer :: i, j
   !$omp simd linear ((i))	! { dg-error "Syntax error" }
   do i = 1, 2
   end do
   !$omp simd linear ()		! { dg-error "Syntax error" }
   do j = 1, 2
   end do
end
