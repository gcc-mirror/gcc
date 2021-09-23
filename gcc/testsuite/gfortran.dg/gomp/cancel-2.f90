! { dg-do compile }

subroutine foo ()
  !$omp parallel
    !$omp cancel parallel if (.true.)
    !$omp cancel parallel if (cancel: .true.)

    !$omp cancel parallel if (.true.) if (.true.)                   ! { dg-error "Duplicated 'if' clause" }
    !$omp cancel parallel if (cancel: .true.) if (cancel: .true.)   ! { dg-error "Failed to match clause" }
    !$omp cancel parallel if (cancel: .true.) if (.true.)           ! { dg-error "IF clause without modifier at .1. used together with IF clauses with modifiers" }
    !$omp cancel parallel if (cancel: .true.) if (parallel: .true.) ! { dg-error "IF clause modifier PARALLEL at .1. not appropriate for the current OpenMP construct" }
    !$omp cancel parallel if (.true.) if (cancel: .true.)           ! { dg-error "Duplicated 'if' clause" }
    !$omp cancel parallel if (parallel: .true.) if (cancel: .true.) ! { dg-error "IF clause modifier PARALLEL at .1. not appropriate for the current OpenMP construct" }
  !$omp end parallel
end subroutine
