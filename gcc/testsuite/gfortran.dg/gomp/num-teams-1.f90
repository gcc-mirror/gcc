module m
  implicit none (type, external)

  interface
  integer function fn(i); integer :: i; end
  end interface

contains

subroutine foo
  !$omp teams num_teams (4 : 6)
  !$omp end teams

  !$omp teams num_teams (7)
  !$omp end teams
end 

subroutine bar
  !$omp target teams num_teams (5 : 19)
  !$omp end target teams

  !$omp target teams num_teams (21)
  !$omp end target teams
end

subroutine baz
  !$omp teams num_teams (fn (1) : fn (2))
  !$omp end teams

  !$omp teams num_teams (fn (3))
  !$omp end teams
end

subroutine qux
  !$omp target teams num_teams (fn (4) : fn (5))
  !$omp end target teams

  !$omp target teams num_teams (fn (6))
  !$omp end target teams
end

subroutine corge
  !$omp target
    !$omp teams num_teams (fn (7) : fn (8))
    !$omp end teams
  !$omp end target

  !$omp target
    !$omp teams num_teams (fn (9))
    !$omp end teams
  !$omp end target
end
end module m
