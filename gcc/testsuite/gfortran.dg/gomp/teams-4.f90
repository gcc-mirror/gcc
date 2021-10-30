! PR middle-end/102972

module m
implicit none (type, external)

! Note: Those are module functions - not an interface
! Hence, they are internally manged to contain the module name!

contains

subroutine omp_set_num_teams (num_teams); integer :: num_teams; end
subroutine omp_set_teams_thread_limit (thread_limit); integer :: thread_limit; end
subroutine omp_set_num_teams_8 (num_teams); integer(8) :: num_teams; end
subroutine omp_set_num_teams_9 (num_teams); integer :: num_teams; end
subroutine omp_set_teams_thread_limit_8 (thread_limit); integer(8) :: thread_limit; end
integer function omp_get_num_teams (); omp_get_num_teams = 0; end
integer function omp_get_team_size (level); integer :: level; omp_get_team_size = 0; end
integer function omp_get_team_num (); omp_get_team_num = 0; end
integer function omp_get_max_teams (); omp_get_max_teams = 0; end
integer function omp_get_teams_thread_limit (); omp_get_teams_thread_limit = 0; end
logical function omp_is_initial_device (); omp_is_initial_device = .true.; end
integer function omp_get_num_threads (); omp_get_num_threads = 0; end
end module

subroutine nest_test ()
  use m
  implicit none (type, external)
  
  integer :: i, n
  !$omp teams
    !$omp distribute parallel do simd
    do i = 1, 64
    end do

    n = 0
    n = n + omp_get_team_size (0)
    n = n + omp_get_num_teams ()
    n = n + omp_get_team_num ()
    call omp_set_num_teams (n)
    call omp_set_num_teams_8 (4_8)
    call omp_set_num_teams_9 (4)
    n = n + omp_get_max_teams ()
    n = n + omp_get_teams_thread_limit ()
    call omp_set_teams_thread_limit (n)
    call omp_set_teams_thread_limit_8 (3_8)
  !$omp end teams
end
