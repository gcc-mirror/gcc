subroutine f
  integer, allocatable :: A1, A2, B(:), C
  !$omp declare target

  !$omp allocators  ! OK
  allocate(A1)

  !$omp allocators allocate(align(8) : a2) ! { dg-error "ALLOCATORS directive at .1. inside a target region must specify an ALLOCATOR modifier for 'a2'" }
  allocate(A2)

  !$omp allocate  ! { dg-error "ALLOCATE directive at .1. inside a target region must specify an ALLOCATOR clause" }
  allocate(B(5))

  !$omp allocate(c)  ! { dg-error "ALLOCATE directive at .1. inside a target region must specify an ALLOCATOR clause for 'c'" }
  allocate(C)
end

subroutine g
  integer, allocatable :: A1, A2, B(:), C

  !$omp target
  !$omp single
    !$omp allocators  ! OK
    allocate(A1)

    !$omp allocators allocate(align(8) : a2) ! { dg-error "ALLOCATORS directive at .1. inside a target region must specify an ALLOCATOR modifier for 'a2'" }
    allocate(A2)

    !$omp allocate  ! { dg-error "ALLOCATE directive at .1. inside a target region must specify an ALLOCATOR clause" }
    allocate(B(5))

    !$omp allocate(c)  ! { dg-error "ALLOCATE directive at .1. inside a target region must specify an ALLOCATOR clause for 'c'" }
    allocate(C)
  !$omp end single
  !$omp end target
end
