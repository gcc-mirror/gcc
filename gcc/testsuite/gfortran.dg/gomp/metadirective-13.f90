! { dg-do compile }

subroutine foo
  implicit none
  external f

  !$omp dispatch
  call f()
  !$omp dispatch
    call f()
  !$omp end dispatch

  !$omp begin metadirective when(construct={parallel} : nothing) otherwise(dispatch)
  call f()
  !$omp end metadirective
end

subroutine bar
  implicit none
  integer :: x
  !$omp atomic update
    x = x + 1
  !$omp atomic update
    x = x + 1
  !$omp end atomic

  !$omp begin metadirective when(construct={parallel} : nothing) otherwise(atomic update)
    x = x + 1
  !$omp end metadirective
end
