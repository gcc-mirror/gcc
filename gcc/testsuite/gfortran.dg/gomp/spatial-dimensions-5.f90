! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

subroutine sub(x)
implicit none
integer :: x, i

!$omp target teams distribute parallel do num_teams(11 : 22) thread_limit(strict,dims(3) : 11,12,13) num_threads(relaxed: 23,45,53) message("first") severity(warning)
do i = 1,1
  x = 0
end do
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-1\\) message\\(.first. \\\[len:5\\\]\\) severity\\(warning\\) thread_limit\\(strict,dims\\(\\):11, 12, 13\\) firstprivate\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(11, 22\\) thread_limit\\(strict,dims\\(\\):11, 12, 13\\) message\\(.first. \\\[len:5\\\]\\) severity\\(warning\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.first. \\\[len:5\\\]\\) severity\\(warning\\) num_threads\\(23, 45, 53\\) shared\\(x\\)" 1 "gimple" } }
end

! { dg-message "sorry, unimplemented: 'num_threads' clause with more than one argument" "" { target *-*-* } 8 }
! { dg-message "sorry, unimplemented: 'message' clause" "" { target *-*-* } 8 }
! { dg-message "sorry, unimplemented: 'thread_limit' clause with 'dims' modifier" "" { target *-*-* } 8 }


subroutine sub2(x)
implicit none
integer :: x, i

!$omp target teams distribute parallel do num_teams(dims(4): 111,222,333, 324) thread_limit(relaxed : 11) num_threads(strict: 445,553) message("second") severity(fatal)
do i = 1,1
  x = 0
end do
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-1\\) message\\(.second. \\\[len:6\\\]\\)severity\\(fatal\\) thread_limit\\(11\\) firstprivate\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):111, 222, 333, 324\\) thread_limit\\(11\\) message\\(.second. \\\[len:6\\\]\\)severity\\(fatal\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.second. \\\[len:6\\\]\\)severity\\(fatal\\) num_threads\\(strict:445, 553\\) shared\\(x\\)" 1 "gimple" } }

!$omp teams distribute parallel do num_teams(11 : 55) thread_limit(strict : 11) num_threads(relaxed: 23,45,53) message("third") severity(warning)
do i = 1,1
  x = 0
end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(.third. \\\[len:5\\\]\\) severity\\(warning\\) num_teams\\(11, 55\\) thread_limit\\(strict:11\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.third. \\\[len:5\\\]\\) severity\\(warning\\) num_threads\\(23, 45, 53\\) shared\\(x\\)" 1 "gimple" } }

!$omp teams distribute parallel do num_teams(dims(2): 1111,33) thread_limit(dims(3) : 11,3,480) num_threads(strict, dims(2): 445,553) message("fourth") severity(fatal)
do i = 1,1
  x = 0
end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(.fourth. \\\[len:6\\\]\\)severity\\(fatal\\) num_teams\\(dims\\(\\):1111, 33\\) thread_limit\\(strict,dims\\(\\):11, 3, 480\\) shared\\(x\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.fourth. \\\[len:6\\\]\\)severity\\(fatal\\) num_threads\\(strict,dims\\(\\):445, 553\\) shared\\(x\\)" 1 "gimple" } }

!$omp target message("tgt1") severity(warning) thread_limit(strict : 1454)
block
  x = 0
end block
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) message\\(.tgt1. \\\[len:4\\\]\\) severity\\(warning\\) thread_limit\\(strict:1454\\) firstprivate\\(x\\)" 1 "gimple" } }

!$omp target message("tgt2") severity(fatal) thread_limit(relaxed : 454)
block
  x = 0
end block
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) message\\(.tgt2. \\\[len:4\\\]\\)severity\\(fatal\\) thread_limit\\(454\\) firstprivate\\(x\\)" 1 "gimple" } }

!$omp teams num_teams(14 : 34) message("teams") severity(fatal)
block
  x = 0
end block
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(.teams. \\\[len:5\\\]\\)severity\\(fatal\\) num_teams\\(14, 34\\) shared\\(x\\)" 1 "gimple" } }

!$omp teams thread_limit(relaxed, dims(2): 514 , 384) severity(warning)
block
  x = 0
end block
! { dg-final { scan-tree-dump-times "#pragma omp teams severity\\(warning\\) thread_limit\\(dims\\(\\):514, 384\\) shared\\(x\\)" 1 "gimple" } }

!$omp parallel num_threads(relaxed, dims(2): 514 , 384) severity(warning)
block
  x = 0
end block
! { dg-final { scan-tree-dump-times "#pragma omp parallel severity\\(warning\\) num_threads\\(dims\\(\\):514, 384\\) shared\\(x\\)" 1 "gimple" } }

!$omp parallel num_threads(strict: 847 , 3523, 53) message("parallel")
block
  x = 0
end block
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(.parallel. \\\[len:8\\\]\\)severity\\(fatal\\) num_threads\\(strict:847, 3523, 53\\) shared\\(x\\)" 1 "gimple" } }

end
