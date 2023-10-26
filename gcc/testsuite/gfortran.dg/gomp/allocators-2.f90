implicit none
integer, allocatable :: a, b
integer :: q
integer :: arr(2)

!$omp allocators allocate(align(64): a)
  allocate(a, b)  ! OK
!$omp end allocators

!$omp allocators allocate(align(128): b)
  allocate(a, b)  ! OK (assuming not allocated)


!$omp allocators allocate(align(62.0): a) ! { dg-error "a scalar positive constant integer alignment expression" }
 allocate(a)


!$omp allocators allocate(align(64): a, b)  ! { dg-error "'b' specified in 'allocate' at \\(1\\) but not in the associated ALLOCATE statement" }
 allocate(a)
!$omp end allocators

end
