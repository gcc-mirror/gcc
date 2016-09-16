! PR fortran/77516
! { dg-do compile }

program pr77516
   integer :: i, x
   x = 0
!$omp simd safelen(0) reduction(+:x)
   do i = 1, 8
      x = x + 1
   end do
   print *, x
end
