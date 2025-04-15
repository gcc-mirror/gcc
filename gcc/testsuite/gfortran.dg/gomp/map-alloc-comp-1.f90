!
! ALLOCATABLE COMPONENTS:
! - OpenMP 5: Permitted (and automatically recursively mapped)
!   -> Not yet supported.
! - OpenMP 4.5: Not permitted.
!
implicit none (type, external)
type sct
  integer, allocatable :: c
end type
type(sct) var

!$omp target enter data map(to:var)
end
