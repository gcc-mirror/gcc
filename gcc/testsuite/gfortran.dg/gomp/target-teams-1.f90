! { dg-additional-options "-fdump-tree-omplower" }

subroutine foo
  implicit none

  !$omp target teams num_teams(dims(2) : 1,2)  ! { dg-message "sorry, unimplemented: 'num_teams' clause with 'dims' modifier" }
  !$omp end target teams

  !$omp target
    !$omp teams num_teams(dims(2) : 2,3)  ! { dg-message "sorry, unimplemented: 'num_teams' clause with 'dims' modifier" }
    !$omp end teams
  !$omp end target

  !$omp teams num_teams(dims(2) : 4,5)  ! { dg-message "sorry, unimplemented: 'num_teams' clause with 'dims' modifier" }
  !$omp end teams

  !$omp target teams num_teams(dims(2) : 5,6)  ! { dg-message "sorry, unimplemented: 'num_teams' clause with 'dims' modifier" }
  !$omp end target teams

  ! -----

  !$omp target thread_limit(dims(3), relaxed : 1,2,3)  ! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'dims' modifier" }
  !$omp end target

  !$omp target thread_limit(dims(3), strict : 2,3,4)  ! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'dims' modifier" }
    !$omp teams
    !$omp end teams
  !$omp end target 

  !$omp target teams thread_limit(dims(3), strict : 3,4,5)  ! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'dims' modifier" }
  !$omp end target teams

  !$omp target
   !$omp teams thread_limit(strict : 456)  ! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'strict' modifier" }
   !$omp end teams
  !$omp end target

  !$omp teams thread_limit(strict : 567)  ! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'strict' modifier" }
  !$omp end teams
end

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(dims\\(\\):1, 2\\) \\\[child fn:" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):1, 2\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(dims\\(\\):2, 3\\) \\\[child fn:" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):2, 3\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):4, 5\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(dims\\(\\):5, 6\\) \\\[child fn:" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):5, 6\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(dims\\(\\):1, 2, 3\\) \\\[child fn:" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(0\\) thread_limit\\(strict,dims\\(\\):2, 3, 4\\) \\\[child fn:" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams\[\\n\\r\]" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(0\\) thread_limit\\(strict,dims\\(\\):3, 4, 5\\) \\\[child fn:" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(strict,dims\\(\\):3, 4, 5\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(0\\) thread_limit\\(strict:456\\) \\\[child fn:" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(strict:456\\)" 1 "omplower" } }

! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(strict:567\\)" 1 "omplower" } }
