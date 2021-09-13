module m
  implicit none (external, type)
  type t
  end type t
contains
subroutine foo (x, y, z, a)
  external :: bar
  type(t) :: x
  integer :: y
  real :: z
  integer :: a(4)

  !$omp masked filter (x)  ! { dg-error "FILTER clause at .1. requires a scalar INTEGER expression" }
    call bar ()
  !$omp end masked

  !$omp masked filter (y)  ! OK
    call bar ()
  !$omp end masked

  !$omp masked filter (z)  ! { dg-error "FILTER clause at .1. requires a scalar INTEGER expression" }
    call bar ()
  !$omp end masked

  !$omp masked filter (a)  ! { dg-error "FILTER clause at .1. requires a scalar INTEGER expression" }
    call bar ()
  !$omp end masked

  !$omp masked filter (0.0)  ! { dg-error "FILTER clause at .1. requires a scalar INTEGER expression" }
    call bar ()
  !$omp end masked

  !$omp masked filter ([1])  ! { dg-error "FILTER clause at .1. requires a scalar INTEGER expression" }
    call bar ()
  !$omp end masked

  !$omp masked filter (-1)  ! { dg-warning "INTEGER expression of FILTER clause at .1. must be non-negative" }
    call bar ()
  !$omp end masked
end
end module

subroutine bar
  !$omp masked filter (0) filter (0)  ! { dg-error "Duplicated 'filter' clause" }
    call foobar
end
