subroutine foo
  implicit none
  external bar
  integer :: i, b(10)
  !$omp task depend(in : bar(1)) ! { dg-error "not a variable" }
  !!$omp end task
  !$omp task depend(out : b(1.0))  ! { dg-warning "Legacy Extension: REAL array index" }
  !$omp end task
  !$omp task depend( iterator( real :: i=1.0:5:1), in : b(i))  ! { dg-error "Expected INTEGER type" }
  !!$omp end task
  !$omp task depend(iterator(i=1.0:5:1), out : b(i))  ! { dg-error "Scalar integer expression for range begin expected" }
  !$omp end task
  !$omp task depend(iterator(i=1:5.0:1), in : b(i))  ! { dg-error "Scalar integer expression for range end expected" }
  !$omp end task
  !$omp task depend(iterator(i=1:5:1.0), in : b(i))  ! { dg-error "Scalar integer expression for range step expected" }
  !$omp end task
  !$omp task depend(iterator(j=1:3:5, i=1:5:0), out : b(i))  ! { dg-error "Nonzero range step expected" }
  !$omp end task
  !$omp task depend(iterator(=1:5:0), in : b(i))  ! { dg-error "Expected identifier" }
  !!$omp end task
  !$omp task depend(iterator(b(2)=1:5:1), in : b(i))  ! { dg-error "Failed to match clause" }
  !!$omp end task
  !$omp task depend(iterator(i=1:5:0, i=4:6), out: b(i))  ! { dg-error "Same identifier 'i' specified again" }
  !!$omp end task
  !$omp task depend(iterator(i=1) ,out: b(i))  ! { dg-error "Expected range-specification" }
  !!$omp end task
end
