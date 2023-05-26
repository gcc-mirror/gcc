implicit none
integer, allocatable :: a, b
integer :: q
integer :: arr(2)

!$omp allocators allocate(align(64): a)
block  ! { dg-error "expected ALLOCATE statement after !.OMP ALLOCATORS" }
end block ! { dg-error "Expecting END PROGRAM statement" }


!$omp allocators allocate(align(64): a)
  allocate(a, b)  ! OK
!$omp end allocators

!$omp allocators allocate(align(128): b)
  allocate(a, b)  ! OK (assuming not allocated)


!$omp allocators allocate(align(64): a)
  allocate(a, b, stat=arr)  ! { dg-error "Stat-variable at .1. must be a scalar INTEGER variable" }
!$omp end allocators


!$omp allocators allocate(align(64): a)
  allocate(q)  ! { dg-error "is neither a data pointer nor an allocatable variable" }
!$omp end allocators ! { dg-error "Unexpected !.OMP END ALLOCATORS" }

end
