module mod
integer i, j

contains
subroutine foo ()
  integer v
  !$omp atomic release
  i = i + 1
  !$omp atomic read
  v = j
end
end module

module m2
!$omp requires atomic_default_mem_order (acq_rel)	! OK
contains
subroutine bar
  !$omp atomic release
  i = i + 1
!$omp requires atomic_default_mem_order (acq_rel)	! { dg-error "must appear in the specification part of a program unit" }
  !$omp atomic read
  v = j
end subroutine
end module m2
