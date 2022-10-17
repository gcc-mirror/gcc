module m
 integer :: omp_all_memory  ! { dg-error "'omp_all_memory', declared at .1., may only be used in the OpenMP DEPEND clause" }
end module m

subroutine f1
  integer :: omp_all_memory  ! { dg-error "'omp_all_memory', declared at .1., may only be used in the OpenMP DEPEND clause" }
  !$omp target depend(out: omp_all_memory)
  !$omp end target
end

subroutine f2
  dimension :: omp_all_memory(5)  ! { dg-error "'omp_all_memory', declared at .1., may only be used in the OpenMP DEPEND clause" }
  !$omp target depend(out: omp_all_memory)
  !$omp end target
end

subroutine f3
  integer :: A
  !$omp target depend(out: omp_all_memory)  ! OK
    omp_all_memory = 5  ! { dg-error "'omp_all_memory', declared at .1., may only be used in the OpenMP DEPEND clause" }
  !$omp end target
end

subroutine f4
  !$omp target map(to: omp_all_memory)  ! { dg-error "'omp_all_memory' at .1. not permitted in this clause" }
  ! !$omp end target

  !$omp task private (omp_all_memory)  ! { dg-error "'omp_all_memory' at .1. not permitted in this clause" }
  ! !$omp end task
end

subroutine f5  ! OK
  !$omp target depend(inout : omp_all_memory )
  !$omp end target

  !$omp target depend ( out : omp_all_memory)
  !$omp end target
end

subroutine f6
  !$omp target depend(in : omp_all_memory )  ! { dg-error "'omp_all_memory' used with DEPEND kind other than OUT or INOUT" }
  ! !$omp end target

  !$omp target depend(mutexinoutset : omp_all_memory )  ! { dg-error "'omp_all_memory' used with DEPEND kind other than OUT or INOUT" }
  ! !$omp end target

  !$omp target depend(inoutset : omp_all_memory )  ! { dg-error "'omp_all_memory' used with DEPEND kind other than OUT or INOUT" }
  ! !$omp end target

  !$omp target depend ( depobj : omp_all_memory)  ! { dg-error "'omp_all_memory' used with DEPEND kind other than OUT or INOUT" }
  !!$omp end target

  !$omp ordered depend ( sink : omp_all_memory)  ! { dg-error "used with dependence-type other than OUT or INOUT" }
end
