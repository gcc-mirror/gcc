! { dg-do compile }

subroutine foo (x)
  integer :: x(5, *)
!$omp parallel
!$omp single
!$omp task depend(in:x(:,5))
!$omp end task
!$omp task depend(in:x(5,:))	! { dg-error "Rightmost upper bound of assumed size array section|proper array section" }
!$omp end task
!$omp end single
!$omp end parallel
end
