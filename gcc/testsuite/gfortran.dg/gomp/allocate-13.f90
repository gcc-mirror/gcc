module m
  implicit none
  !$omp requires dynamic_allocators
contains
subroutine f ()
  !$omp declare target
  integer :: var
  !$omp allocate(var)
  var = 5
end

subroutine h ()
  !$omp target
   !$omp parallel
    !$omp single
      block
       integer :: var2(5)
       !$omp allocate(var2)
       var2(1) = 7
      end block
    !$omp end single
   !$omp end parallel
  !$omp end target
end
end module
