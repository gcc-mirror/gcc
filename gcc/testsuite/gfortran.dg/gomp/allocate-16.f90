integer, pointer :: ptr

!$omp flush
!$omp allocate(ptr)
allocate(ptr)
end

! { dg-error "'!.OMP ALLOCATE' at .1. requires '-fopenmp-allocators'" "" { target *-*-* } 4 }
! { dg-warning "All files that might deallocate such a variable must be compiled with '-fopenmp-allocators'" "" { target *-*-* } 4 }
! { dg-note "This includes explicit DEALLOCATE, reallocation on intrinsic assignment, INTENT\\(OUT\\) for allocatable dummy arguments, and reallocation of allocatable components allocated with an OpenMP allocator" "" { target *-*-* } 0 }
