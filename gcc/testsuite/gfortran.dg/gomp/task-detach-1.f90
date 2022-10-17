! { dg-do compile }
! { dg-options "-fopenmp" }

program task_detach_1
  use iso_c_binding, only: c_intptr_t
  implicit none
  
  integer, parameter :: omp_event_handle_kind = c_intptr_t
  integer (kind=omp_event_handle_kind) :: x, y
  integer(1) :: z
  
  !$omp task detach(x) detach(y) ! { dg-error "Failed to match clause at \\\(1\\\)" }
  !$omp end task ! { dg-error "Unexpected !\\\$OMP END TASK statement at \\\(1\\\)" }

  !$omp task mergeable detach(x) ! { dg-error "'DETACH' clause at \\\(1\\\) must not be used together with 'MERGEABLE' clause" }
  !$omp end task

  !$omp task detach(x) mergeable ! { dg-error "'DETACH' clause at \\\(1\\\) must not be used together with 'MERGEABLE' clause" }
  !$omp end task

  !$omp task detach(z) ! { dg-error "'z' at \\\(1\\\) should be a scalar of type integer\\\(kind=omp_event_handle_kind\\\)" }
  !$omp end task
  
  !$omp task detach (x) firstprivate (x) ! { dg-error "DETACH event handle 'x' in FIRSTPRIVATE clause at \\\(1\\\)" }
  !$omp end task

  !$omp task detach (x) shared (x) ! { dg-error "DETACH event handle 'x' in SHARED clause at \\\(1\\\)" }
  !$omp end task
end program
