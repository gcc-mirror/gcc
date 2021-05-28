subroutine foo
  implicit none
  external bar
  integer :: i, b(10)
  !$omp task affinity(bar(1)) ! { dg-error "not a variable" }
  !!$omp end task
  !$omp task affinity(b(1.0))  ! { dg-warning "Legacy Extension: REAL array index" }
  !$omp end task
  !$omp task affinity( iterator( real :: i=1.0:5:1) : b(i))  ! { dg-error "Expected INTEGER type" }
  !!$omp end task
  !$omp task affinity(iterator(i=1.0:5:1) : b(i))  ! { dg-error "Scalar integer expression for range begin expected" }
  !$omp end task
  !$omp task affinity(iterator(i=1:5.0:1) : b(i))  ! { dg-error "Scalar integer expression for range end expected" }
  !$omp end task
  !$omp task affinity(iterator(i=1:5:1.0) : b(i))  ! { dg-error "Scalar integer expression for range step expected" }
  !$omp end task
  !$omp task affinity(iterator(j=1:3:5, i=1:5:0) : b(i))  ! { dg-error "Nonzero range step expected" }
  !$omp end task
  !$omp task affinity(iterator(=1:5:0) : b(i))  ! { dg-error "31:Syntax error in OpenMP variable list" }
  !!$omp end task
  !$omp task affinity(iterator(b(2)=1:5:0) : b(i))  ! { dg-error "31:Syntax error in OpenMP variable list" }
  !!$omp end task
  !$omp task affinity(iterator(i=1:5:0, i=4:6) : b(i))  ! { dg-error "Same identifier 'i' specified again" }
  !!$omp end task
  !$omp task affinity(iterator(i=1) : b(i))  ! { dg-error "Expected range-specification" }
  !!$omp end task
end
