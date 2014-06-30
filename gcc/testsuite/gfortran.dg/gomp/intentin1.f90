! { dg-do compile }

subroutine foo (x)
  integer, pointer, intent (in) :: x
  integer :: i
!$omp parallel private (x)		! { dg-error "INTENT.IN. POINTER" }
!$omp end parallel
!$omp parallel do lastprivate (x)	! { dg-error "INTENT.IN. POINTER" }
  do i = 1, 10
  end do
!$omp simd linear (x)			! { dg-error "INTENT.IN. POINTER" }
  do i = 1, 10
  end do
!$omp single				! { dg-error "INTENT.IN. POINTER" }
!$omp end single copyprivate (x)
end
