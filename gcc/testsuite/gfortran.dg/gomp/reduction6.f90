! { dg-do compile }

implicit none
integer :: a, b, i
a = 0

!$omp simd reduction(inscan,+:a)  ! { dg-error "31: With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" }
do i=1,10
  a = a + 1
end do

!$omp parallel
!$omp do reduction(inscan,+:a)  ! { dg-error "29: With INSCAN at .1., expected loop body with ..OMP SCAN between two structured block sequences" }
do i=1,10
  a = a + 1
end do
!$omp end parallel
end
