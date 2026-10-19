! { dg-do compile }

implicit none

integer, parameter :: M = 1024
integer :: N, A(1)

N = 1024

!$omp target dyn_groupprivate(0)  ! OK, zero is permitted
block; end block

!$omp target dyn_groupprivate(0)  dyn_groupprivate(0)  ! { dg-error "Duplicated 'dyn_groupprivate' clause" }
block; end block

!$omp target dyn_groupprivate(-123)  ! { dg-warning "INTEGER expression of DYN_GROUPPRIVATE clause at .1. must be non-negative \\\[-Wopenmp\\\]" }
block; end block

!$omp target dyn_groupprivate (0 * M-1)  ! { dg-warning "INTEGER expression of DYN_GROUPPRIVATE clause at .1. must be non-negative \\\[-Wopenmp\\\]" }
block; end block

!$omp target dyn_groupprivate ( fallback ( other ) : N)  ! { dg-error "Expected 'abort', 'default_mem' or 'null' followed by '\\)' at .1." }
block; end block

!$omp target dyn_groupprivate ( A )  ! { dg-error "DYN_GROUPPRIVATE clause at .1. requires a scalar INTEGER expression" }
block; end block

!$omp target dyn_groupprivate ( 1024. )  ! { dg-error "DYN_GROUPPRIVATE clause at .1. requires a scalar INTEGER expression" }
block; end block

!$omp target dyn_groupprivate ( fallback(null) cgroup: 1024. )  ! { dg-error "Expected '\\)' at .1." }
block; end block

!$omp target dyn_groupprivate ( fallback(default_mem),thread: 1024. )  ! { dg-error "Expected 'cgroup' or 'fallback' clause at .1." }
block; end block

!$omp target dyn_groupprivate ( cgroup,fallback(null),cgroup: 1024. )  ! { dg-error "Duplicated 'cgroup' modifier at .1." }
block; end block

!$omp target dyn_groupprivate ( fallback(abort),cgroup, fallback(abort): 1024. )  ! { dg-error "Duplicated 'fallback' modifier at .1." }
block; end block
end
