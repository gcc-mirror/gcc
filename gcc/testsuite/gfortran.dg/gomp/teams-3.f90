! PR middle-end/102972

module m
implicit none (type, external)
interface
subroutine omp_set_num_teams (num_teams); integer :: num_teams; end
subroutine omp_set_teams_thread_limit (thread_limit); integer :: thread_limit; end
subroutine omp_set_num_teams_8 (num_teams); integer(8) :: num_teams; end
subroutine omp_set_num_teams_9 (num_teams); integer :: num_teams; end
subroutine omp_set_teams_thread_limit_8 (thread_limit); integer(8) :: thread_limit; end
integer function omp_get_num_teams (); end
integer function omp_get_team_size (level); integer :: level; end
integer function omp_get_team_num (); end
integer function omp_get_max_teams (); end
integer function omp_get_teams_thread_limit (); end
logical function omp_is_initial_device (); end
integer function omp_get_num_threads (); end
end interface

contains

subroutine valid ()
  integer :: i, n
  !$omp teams
    !$omp distribute
    do i = 1, 64
    end do

    n = omp_get_num_teams ()
    if (n >= omp_get_team_num ()) &
      error stop

    !$omp parallel do
    do i = 1, 64
      if (.not.omp_is_initial_device () .or. omp_get_num_threads () < 0) &
        error stop
    end do

    !$omp loop
    do i = 1, 64
    end do
  !$omp end teams
end

subroutine invalid_nest ()
  integer :: i, n
  !$omp teams
    !$omp distribute parallel do simd
    do i = 1, 64
    end do

    n = 0
    n = n + omp_get_team_size (0)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_team_size\[^\n\r]*' strictly nested in a 'teams' region" }
    n = n + omp_get_num_teams ()
    n = n + omp_get_team_num ()
    call omp_set_num_teams (n)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_set_num_teams\[^\n\r]*' strictly nested in a 'teams' region" }
    call omp_set_num_teams_8 (4_8)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_set_num_teams_8\[^\n\r]*' strictly nested in a 'teams' region" }
    call omp_set_num_teams_9 (4)  ! OK - but misnamed user function
    n = n + omp_get_max_teams ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_max_teams\[^\n\r]*' strictly nested in a 'teams' region" }
    n = n + omp_get_teams_thread_limit ()  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_teams_thread_limit\[^\n\r]*' strictly nested in a 'teams' region" }
    call omp_set_teams_thread_limit (n)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_set_teams_thread_limit'\[^\n\r]* strictly nested in a 'teams' region" }
    call omp_set_teams_thread_limit_8 (3_8)  ! { dg-error "OpenMP runtime API call '\[^\n\r]*omp_set_teams_thread_limit_8'\[^\n\r]* strictly nested in a 'teams' region" }
  !$omp end teams
end
end module
