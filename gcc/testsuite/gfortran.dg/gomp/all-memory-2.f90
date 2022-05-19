! { dg-additional-options "-fno-openmp" }
module m
 integer :: omp_all_memory
end module m

subroutine f1
  integer :: omp_all_memory
  !$omp target depend(out: omp_all_memory)
  !$omp end target
end

subroutine f2
  dimension :: omp_all_memory(5)
  !$omp target depend(out: omp_all_memory)
  !$omp end target
end

subroutine f3
  integer :: A
  !$omp target depend(out: omp_all_memory)
    omp_all_memory = 5
  !$omp end target
end

subroutine f4
  !$omp target map(to: omp_all_memory)
  ! !$omp end target

  !$omp task private (omp_all_memory)
  ! !$omp end task
end

subroutine f5
  !$omp target depend(inout : omp_all_memory )
  !$omp end target

  !$omp target depend ( out : omp_all_memory)
  !$omp end target
end

subroutine f6
  !$omp target depend(in : omp_all_memory )
  ! !$omp end target

  !$omp target depend(mutexinoutset : omp_all_memory )
  ! !$omp end target

  !$omp target depend(inoutset : omp_all_memory )
  ! !$omp end target

 !$omp target depend ( depobj : omp_all_memory)
 !$omp end target

 !$omp ordered depend ( sink : omp_all_memory)
end
