! { dg-do compile }
! { dg-additional-options "-O0 -fdump-tree-gimple -fdump-tree-omplower -fdump-tree-ompexp" }

subroutine one_a(x)
implicit none
integer :: x
!$omp teams num_teams(dims(3) : 1,2,3) &
!$omp&      thread_limit ( dims ( 4 ) : 1, 2, 3, 4)
! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'dims' modifier" "" { target *-*-* } .-1 }
! { dg-message "sorry, unimplemented: 'num_teams' clause with 'dims' modifier" "" { target *-*-* } .-2 }
!$omp   parallel num_threads(dims(3): 1,2,3)        ! { dg-message "sorry, unimplemented: 'num_threads' clause with 'dims' modifier" }
  x = 1
!$omp   end parallel
!$omp end teams
end

! As the error is printed in 'omp-expand.cc', it is shown only once and not for all of the following
! cases. This also affects omp-lower. Hence:
! { dg-final { scan-tree-dump-times "#pragma omp teams" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_teams_reg " 1 "ompexp" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "ompexp" } }

subroutine one_b(x)
implicit none
integer :: x
!$omp teams num_teams(dims(3) : 1,2,3) &
!$omp&      thread_limit ( dims ( 4 ) , relaxed : 1, 2, 3, 4)
!$omp   parallel num_threads(dims(3) , relaxed : 1,2,3)
  x = 1
!$omp   end parallel
!$omp end teams
end

subroutine one_c(x)
implicit none
integer :: x
!$omp teams num_teams(dims(3) : 11,22,33) &
!$omp&      thread_limit ( dims ( 4 ) , strict : 11, 22, 33, 44)
!$omp   parallel num_threads(dims(3) , strict : 11,22,33)
  x = 1
!$omp   end parallel
!$omp end teams
end

subroutine one_d(x)
implicit none
integer :: x
!$omp teams num_teams ( dims(3) : 1,2,3) &
!$omp&      thread_limit ( relaxed ,  dims ( 4 ) : 111, 222, 333, 444)
!$omp   parallel num_threads( relaxed , dims(3) : 111,222,333)
  x = 1
!$omp   end parallel
!$omp end teams
end

subroutine one_e(x)
implicit none
integer :: x
!$omp teams num_teams ( dims(3) : 1,2,3) &
!$omp&      thread_limit ( strict ,  dims ( 4 ) : 1111, 2222, 3333, 4444)
!$omp   parallel num_threads( strict , dims(3) : 1111,2222,3333)
  x = 1
!$omp   end parallel
!$omp end teams
end


subroutine two(x)
implicit none
integer :: x
!$omp teams thread_limit (strict : 4)
!$omp   parallel num_threads(strict : 2)
  x = 1
!$omp   end parallel
!$omp end teams
end

subroutine zero(x)
implicit none
integer :: x
!$omp teams num_teams(1234) thread_limit ( 2345)  ! OK - old code
!$omp   parallel num_threads( 3456)               ! OK - old code
  x = 1
!$omp   end parallel
!$omp end teams

!$omp teams num_teams(123:456)
  x = 1
!$omp end teams
end

subroutine three(x)
implicit none
integer :: x
!$omp teams num_teams(4567) thread_limit (relaxed : 5678)  ! OK - old code + 'relaxed'
!$omp   parallel num_threads(relaxed : 6789)               ! OK - old code + 'relaxed'
  x = 1
!$omp   end parallel
!$omp end teams
end

subroutine four(x)
implicit none
integer :: x
!$omp parallel num_threads(1 , 2 , 3 )
  x = 1
!$omp end parallel
end

subroutine five(x)
implicit none
integer :: x
!$omp parallel num_threads(relaxed : 11,22,333)
  x = 1
!$omp end parallel
end

subroutine six(x)
implicit none
integer :: x
!$omp parallel num_threads(strict : 111,222,333)
  x = 1
!$omp end parallel
end


subroutine seven(x)
implicit none
integer :: x
  integer :: static, relaxed, dims(2)
  !$omp teams num_teams(static) thread_limit (static)  ! OK - old code (using variable name that looks like a modifier name)
  !$omp   parallel num_threads(static)                 ! OK
    x = 1
  !$omp   end parallel
  !$omp end teams

  !$omp teams num_teams(relaxed) thread_limit (relaxed)  ! OK
  !$omp   parallel num_threads(relaxed)                  ! OK
    x = 1
  !$omp   end parallel
  !$omp end teams

  !$omp teams num_teams(dims(2)) thread_limit (dims(2))  ! OK
  !$omp   parallel num_threads(dims(2))                  ! OK
    x = 1
  !$omp   end parallel
  !$omp end teams

end subroutine

! Check that the tree is correctly generated (gimple tree)

! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):1, 2, 3\\) thread_limit\\(strict,dims\\(\\):1, 2, 3, 4\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(strict,dims\\(\\):1, 2, 3\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):1, 2, 3\\) thread_limit\\(dims\\(\\):1, 2, 3, 4\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(dims\\(\\):1, 2, 3\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):11, 22, 33\\) thread_limit\\(strict,dims\\(\\):11, 22, 33, 44\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(strict,dims\\(\\):11, 22, 33\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):1, 2, 3\\) thread_limit\\(dims\\(\\):111, 222, 333, 444\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(dims\\(\\):111, 222, 333\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):1, 2, 3\\) thread_limit\\(strict,dims\\(\\):1111, 2222, 3333, 4444\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(strict,dims\\(\\):1111, 2222, 3333\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(strict:4\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(strict:2\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(1234\\) thread_limit\\(2345\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(3456\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(123, 456\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(4567\\) thread_limit\\(5678\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(6789\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(1, 2, 3\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(11, 22, 333\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(strict:111, 222, 333\\) shared\\(x\\)" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = static;" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = relaxed;" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = dims\\\[1\\\];" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(_\[0-9\]+\\) thread_limit\\(_\[0-9\]+\\) shared\\(static\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(_\[0-9\]+\\) thread_limit\\(_\[0-9\]+\\) shared\\(relaxed\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(_\[0-9\]+\\) thread_limit\\(_\[0-9\]+\\) shared\\(dims\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel num_threads\\(D\\.\[0-9\]+\\) shared\\(x\\)" 3 "gimple" } }
