! { dg-do compile }
! { dg-options "-fopenmp" }

program p
  use iso_c_binding, only: c_intptr_t
  implicit none
  integer, parameter :: omp_event_handle_kind = c_intptr_t

  type dt
    integer(omp_event_handle_kind) :: f
  end type
  integer(omp_event_handle_kind) :: x(1)
  type(dt) :: y

  !$omp task detach(x) ! { dg-error "'x' at \\\(1\\\) should be a scalar of type integer\\\(kind=omp_event_handle_kind\\\)" }
  !$omp end task

  !$omp task detach(x(1)) ! { dg-error "The event handle at \\\(1\\\) must not be an array element" }
  !$omp end task

  !$omp task detach(y) ! { dg-error "'y' at \\\(1\\\) should be a scalar of type integer\\\(kind=omp_event_handle_kind\\\)" }
  !$omp end task

  !$omp task detach(y%f) ! { dg-error "The event handle at \\\(1\\\) must not be part of a derived type or class" }
  !$omp end task
end program
