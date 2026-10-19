! PR fortran/77516
! { dg-do compile }

program pr77516
   integer :: i, x
   x = 0
!$omp simd safelen(0) reduction(+:x)	! { dg-error "INTEGER expression of SAFELEN clause at .1. must be positive and constant" }
   do i = 1, 8
      x = x + 1
   end do
   print *, x
end
