module m
  implicit none
  integer i
contains

subroutine f_omp_parallel
 !$omp parallel
  !$omp parallel
  !$omp end parallel

  !$omp target
  !$omp end target

  !$omp target data map(i)
  !$omp end target data

  !$omp target update to(i)

  !$omp target data map(i)
    !$omp parallel
    !$omp end parallel

    !$omp target
    !$omp end target

    !$omp target data map(i)
    !$omp end target data

    !$omp target update to(i)
  !$omp end target data
 !$omp end parallel
end

subroutine f_omp_target
  !$omp target
    !$omp parallel
    !$omp end parallel
  !$omp end target
end

subroutine f_omp_target_data
 !$omp target data map(i)
  !$omp parallel
  !$omp end parallel

  !$omp target
  !$omp end target

  !$omp target data map(i)
  !$omp end target data

  !$omp target update to(i)

  !$omp target data map(i)
    !$omp parallel
    !$omp end parallel

    !$omp target
    !$omp end target

    !$omp target data map(i)
    !$omp end target data

    !$omp target update to(i)
  !$omp end target data
 !$omp end target data
end
end module m
