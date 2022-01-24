module m
  implicit none (type, external)

contains

subroutine foo (i)
  integer :: i

  !$omp teams num_teams (6 : 4)		! { dg-warning "NUM_TEAMS lower bound at .1. larger than upper bound at .2." }
  !$omp end teams

  !$omp teams num_teams (-7)		! { dg-warning "INTEGER expression of NUM_TEAMS clause at .1. must be positive" }
  !$omp end teams

  !$omp teams num_teams (i : -7)		! { dg-warning "INTEGER expression of NUM_TEAMS clause at .1. must be positive" }
  !$omp end teams

  !$omp teams num_teams (-7 : 8)		! { dg-warning "INTEGER expression of NUM_TEAMS clause at .1. must be positive" }
  !$omp end teams
end

subroutine bar (i)
  integer :: i

  !$omp target teams num_teams (6 : 4)	! { dg-warning "NUM_TEAMS lower bound at .1. larger than upper bound at .2." }
  !$omp end target teams

  !$omp target teams num_teams (-7)	! { dg-warning "INTEGER expression of NUM_TEAMS clause at .1. must be positive" }
  !$omp end target teams

  !$omp target teams num_teams (i : -7)	! { dg-warning "INTEGER expression of NUM_TEAMS clause at .1. must be positive" }
  !$omp end target teams

  !$omp target teams num_teams (-7 : 8)	! { dg-warning "INTEGER expression of NUM_TEAMS clause at .1. must be positive" }
  !$omp end target teams
end
end module
