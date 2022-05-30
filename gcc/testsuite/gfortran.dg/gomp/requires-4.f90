subroutine bar
!$omp requires unified_shared_memory,unified_address,reverse_offload
end

module m
!$omp requires unified_shared_memory,unified_address,reverse_offload
end module m

subroutine foo
  !$omp target
  !$omp end target
! { dg-error "OpenMP device constructs/routines but does not set !.OMP REQUIRES REVERSE_OFFLOAD but other program units do" "" { target *-*-* } 9 }
! { dg-error "OpenMP device constructs/routines but does not set !.OMP REQUIRES UNIFIED_ADDRESS but other program units do" "" { target *-*-* } 9 }
! { dg-error "OpenMP device constructs/routines but does not set !.OMP REQUIRES UNIFIED_SHARED_MEMORY but other program units do" "" { target *-*-* } 9 }
end

subroutine foobar
i = 5  ! < execution statement
!$omp requires atomic_default_mem_order(seq_cst) ! { dg-error "Unexpected ..OMP REQUIRES statement" }
end

program main
!$omp requires dynamic_allocators ! OK
!$omp requires unified_shared_memory
!$omp requires unified_address
!$omp requires reverse_offload
contains
  subroutine foo
    !$omp target
    !$omp end target
  end subroutine
  subroutine bar
    !$omp requires unified_address ! { dg-error "must appear in the specification part of a program unit" }
  end subroutine bar
end
! { dg-prune-output "not yet supported" }
