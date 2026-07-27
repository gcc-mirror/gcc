! { dg-do compile }
! { dg-additional-options "-fdump-tree-omplower -fdump-tree-ompexp" }

! For ompexp, only check teams + parallel and not target (teams)

! Note: The following tests code that produces identical code to 5.x
! even though OpenMP 6.0 features are used. Namely:
! - relaxed  modifier
! - relaxed, dims(1)  modiefiers
! - message clause with parallel and teams - if there is num_teams clause
!   (For target (teams), there is an implicit 'num_teams'; to avoid
!    complex sorry code, message + target (teams) is rejected.)

subroutine sub(x)
implicit none
integer :: x, i

!$omp target thread_limit(relaxed : 11)
block
  x = 1
end block
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(11\\) firstprivate\\(x\\) \\\[child fn" 1 "omplower" } }

!$omp target parallel do simd thread_limit(relaxed, dims(1) : 22)
do i = 1, 1
  x = 1
end do
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(dims\\(\\):22\\) map\\(tofrom:i \\\[len: 4\\\] \\\[runtime_implicit\\\]\\) firstprivate\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel lastprivate\\(i\\) shared\\(x\\) \\\[child fn" 1 "omplower" } }

!$omp target teams thread_limit(relaxed : 31) num_teams(dims(1) : 33)
block
  x = 1
end block
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-1\\) thread_limit\\(31\\) firstprivate\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):33\\) thread_limit\\(31\\) shared\\(x\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_teams4 \\(33, 33, 31, D.\[0-9]+\\);" 1 "ompexp" } }


!$omp target teams distribute parallel do simd thread_limit(relaxed : 41) num_teams(dims(1) : 43) num_threads( dims ( 1 ) , relaxed : 44)
do i = 1, 1
  x = 1
end do
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-1\\) thread_limit\\(41\\) map\\(tofrom:i \\\[len: 4\\\] \\\[runtime_implicit\\\]\\) firstprivate\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):43\\) thread_limit\\(41\\) shared\\(i\\) shared\\(x\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel _looptemp_\\(D.\[0-9\]+\\) _looptemp_\\(D.\[0-9\]+\\) num_threads\\(dims\\(\\):44\\) lastprivate\\(i\\) shared\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel \\(sub_._omp_fn.\[0-9]+, &.omp_data_o.\[0-9]+, 44, 0\\);" 1 "ompexp" } }

!$omp teams thread_limit(relaxed : 51) num_teams(dims(1) : 53)
block
end block
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):53\\) thread_limit\\(51\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_teams_reg \\(sub_._omp_fn.\[0-9\]+, 0B, 53, 51, 0\\);" 1 "ompexp" } }

!$omp teams distribute parallel do simd thread_limit(relaxed : 61) num_teams(dims(1) : 63) num_threads(relaxed, dims(1) : 64)
do i = 1, 1
  x = 1
end do
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):63\\) thread_limit\\(61\\) shared\\(i\\) shared\\(x\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel _looptemp_\\(D.\[0-9\]+\\) _looptemp_\\(D.\[0-9\]+\\) num_threads\\(dims\\(\\):64\\) lastprivate\\(i\\) shared\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_teams_reg \\(sub_._omp_fn.\[0-9\]+, &.omp_data_o.\[0-9\]+, 63, 61, 0\\);" 1 "ompexp" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel \\(sub_._omp_fn.\[0-9\]+, &.omp_data_o.\[0-9\]+, 64, 0\\);" 1 "ompexp" } }

!$omp parallel num_threads(relaxed, dims(1) : 77) message("my77") severity(fatal)
block
  x = 1
end block
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.my77. \\\[len:4\\\]\\)severity\\(fatal\\) num_threads\\(dims\\(\\):77\\) shared\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel \\(sub_._omp_fn.\[0-9\]+, &.omp_data_o.\[0-9\]+, 77, 0\\);" 1 "ompexp" } }


!$omp parallel num_threads(relaxed : 88) message("my78") severity(warning)
block
  x = 1
end block
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.my78. \\\[len:4\\\]\\) severity\\(warning\\) num_threads\\(88\\) shared\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel \\(sub_._omp_fn.\[0-9\]+, &.omp_data_o.\[0-9\]+, 88, 0\\);" 1 "ompexp" } }

!$omp teams thread_limit(relaxed : 91) num_teams(1 : 93)
block
  x = 1
end block
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(1, 93\\) thread_limit\\(91\\) shared\\(x\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_teams_reg \\(sub_._omp_fn.\[0-9\]+, &.omp_data_o.\[0-9\]+, 93, 91, 0\\);" 1 "ompexp" } }

!$omp target teams thread_limit(relaxed : 991) num_teams(191 : 993)
block
  x = 1
end block
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-1\\) thread_limit\\(991\\) firstprivate\\(x\\) \\\[child fn" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(191, 993\\) thread_limit\\(991\\) shared\\(x\\)" 1 "omplower" } }

!$omp teams thread_limit(relaxed : 1231) message("my1234") severity(warning)
block
end block 
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(.my1234. \\\[len:6\\\]\\) severity\\(warning\\) thread_limit\\(1231\\)" 1 "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_teams_reg \\(sub_._omp_fn.\[0-9\]+, 0B, 0, 1231, 0\\);" 1 "ompexp" } }

end
