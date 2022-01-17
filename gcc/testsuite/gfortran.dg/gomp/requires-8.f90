module m  !  { dg-error "has OpenMP device constructs/routines but does not set !.OMP REQUIRES UNIFIED_SHARED_MEMORY but other program units do" }
  !$omp requires reverse_offload
contains
 subroutine foo
  interface
   subroutine bar2
     !$omp requires dynamic_allocators
   end subroutine
  end interface
  !$omp target
     call bar2()
  !$omp end target
 end subroutine foo
end module m

subroutine bar  ! { dg-error "has OpenMP device constructs/routines but does not set !.OMP REQUIRES REVERSE_OFFSET but other program units do" }
  !use m
  !$omp requires unified_shared_memory
  !$omp declare target
end subroutine bar

! { dg-prune-output "not yet supported" }
