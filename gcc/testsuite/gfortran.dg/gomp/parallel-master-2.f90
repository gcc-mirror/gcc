  use iso_c_binding, only: c_intptr_t
  implicit none (external, type)
  integer, parameter :: omp_event_handle_kind = c_intptr_t
  integer (kind=omp_event_handle_kind)  :: x
  !$omp parallel master default (none) ! { dg-message "enclosing 'parallel'" }
    !$omp task detach (x) ! { dg-error "'x' not specified in enclosing 'parallel'" }
    !$omp end task
  !$omp end parallel master
end 
