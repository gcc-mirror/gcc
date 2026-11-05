! { dg-additional-options "-fdump-tree-original" }

subroutine f(n)
 integer, value :: n
 integer :: i

 ! Clauses only on target (parallel also accepts 'severity')

 !$omp target parallel do simd thread_limit(dims(2), strict : 1,2) dyn_groupprivate(fallback(null), cgroup : 123) message("one") severity(warning) ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"one\" \\\[len:3\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):123\\) thread_limit\\(strict,dims\\(\\):1, 2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"one\" \\\[len:3\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target parallel do thread_limit(dims(2), relaxed : 2,3) dyn_groupprivate(fallback(abort), cgroup : 234) message("two") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"two\" \\\[len:3\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(abort\\):234\\) thread_limit\\(dims\\(\\):2, 3\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"two\" \\\[len:3\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target parallel loop thread_limit(relaxed: 34) dyn_groupprivate(fallback(default_mem), cgroup : 345) message("three") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"three\" \\\[len:5\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(default_mem\\):345\\) thread_limit\\(34\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"three\" \\\[len:5\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target parallel thread_limit(strict : 45) dyn_groupprivate(cgroup : 456) message("four") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 block
 end block
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"four\" \\\[len:4\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(456\\) thread_limit\\(strict:45\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"four\" \\\[len:4\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target simd thread_limit(dims(1) : 67) dyn_groupprivate(fallback(abort), cgroup : 678) message("five")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"five\" \\\[len:4\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(abort\\):678\\) thread_limit\\(strict,dims\\(\\):67\\)" 1 "original" } }

 ! Clauses on target teams (additionally with num_teams)

 !$omp target teams distribute parallel do simd num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("seven") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"seven\" \\\[len:5\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) thread_limit\\(strict:331\\) num_teams\\(11:22\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(strict:331\\) num_teams\\(11:22\\) message\\(\"seven\" \\\[len:5\\\]\\) severity\\(fatal\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"seven\" \\\[len:5\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target teams distribute parallel do num_teams(dims(1):1343) thread_limit(relaxed : 1452) dyn_groupprivate( cgroup ,fallback(default_mem) : 1248) message("nine") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"nine\" \\\[len:4\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(default_mem\\):1248\\) thread_limit\\(1452\\) num_teams\\(dims\\(\\):1343\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(1452\\) num_teams\\(dims\\(\\):1343\\) message\\(\"nine\" \\\[len:4\\\]\\) severity\\(fatal\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"nine\" \\\[len:4\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target teams distribute simd num_teams(dims(2):3,3) thread_limit(938) dyn_groupprivate( 1458) message("ten")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"ten\" \\\[len:3\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(1458\\) thread_limit\\(938\\) num_teams\\(dims\\(\\):3, 3\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(938\\) num_teams\\(dims\\(\\):3, 3\\) message\\(\"ten\" \\\[len:3\\\]\\) severity\\(fatal\\)" 1 "original" } }

 !$omp target teams distribute num_teams(4223) thread_limit(729) dyn_groupprivate( 803) message("eleven")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"eleven\" \\\[len:6\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(803\\) thread_limit\\(729\\) num_teams\\(4223\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(729\\) num_teams\\(4223\\) message\\(\"eleven\" \\\[len:6\\\]\\) severity\\(fatal\\)" 1 "original" } }
!
 !$omp target teams loop num_teams(444) thread_limit(451) dyn_groupprivate( 842) message("twelf")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"twelf\" \\\[len:5\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(842\\) thread_limit\\(451\\) num_teams\\(444\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams thread_limit\\(451\\) num_teams\\(444\\) message\\(\"twelf\" \\\[len:5\\\]\\) severity\\(fatal\\)" 1 "original" } }


 ! Same, but in two directives - put on 'teams'

 !$omp target
 !$omp teams distribute parallel do simd num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("13") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(null\\):243\\) thread_limit\\(strict:331\\) num_teams\\(11:22\\)\[\\n\\r\{ \]+#pragma omp teams thread_limit\\(strict:331\\) num_teams\\(11:22\\) message\\(\"13\" \\\[len:2\\\]\\) severity\\(fatal\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"13\" \\\[len:2\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target
 !$omp teams distribute parallel do num_teams(dims(1):1343) thread_limit(relaxed : 1452) dyn_groupprivate( cgroup ,fallback(default_mem) : 1248) message("14") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(default_mem\\):1248\\) thread_limit\\(1452\\) num_teams\\(dims\\(\\):1343\\)\[\\n\\r\{ \]+#pragma omp teams thread_limit\\(1452\\) num_teams\\(dims\\(\\):1343\\) message\\(\"14\" \\\[len:2\\\]\\) severity\\(fatal\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"14\" \\\[len:2\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp target
 !$omp teams distribute simd num_teams(dims(2):3,3) thread_limit(938) dyn_groupprivate( 1458) message("15")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(1458\\) thread_limit\\(938\\) num_teams\\(dims\\(\\):3, 3\\)\[\\n\\r\{ \]+#pragma omp teams thread_limit\\(938\\) num_teams\\(dims\\(\\):3, 3\\) message\\(\"15\" \\\[len:2\\\]\\) severity\\(fatal\\)" 1 "original" } }

 !$omp target
 !$omp teams distribute num_teams(4223) thread_limit(729) dyn_groupprivate( 803) message("16")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(803\\) thread_limit\\(729\\) num_teams\\(4223\\)\[\\n\\r\{ \]+#pragma omp teams thread_limit\\(729\\) num_teams\\(4223\\) message\\(\"16\" \\\[len:2\\\]\\) severity\\(fatal\\)" 1 "original" } }

 !$omp target
 !$omp teams loop num_teams(444) thread_limit(451) dyn_groupprivate( 842) message("17")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(842\\) thread_limit\\(451\\) num_teams\\(444\\)\[\\n\\r\{ \]+#pragma omp teams thread_limit\\(451\\) num_teams\\(444\\) message\\(\"17\" \\\[len:2\\\]\\) severity\\(fatal\\)" 1 "original" } }


 ! Same but in two directives, put on target, except for num_teams

 !$omp target thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("18")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 !$omp teams distribute parallel do simd num_teams(11:22) severity(warning)
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"18\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) thread_limit\\(strict:331\\) num_teams\\(11:22\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(11:22\\)" 1 "original" } }

 !$omp target thread_limit(relaxed : 1452) dyn_groupprivate( cgroup ,fallback(default_mem) : 1248) message("19")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 !$omp teams distribute parallel do num_teams(dims(1):1343) severity(warning)
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"19\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(default_mem\\):1248\\) thread_limit\\(1452\\) num_teams\\(dims\\(\\):1343\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):1343\\)" 1 "original" } }

 !$omp target thread_limit(938) dyn_groupprivate( 1458) message("20")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 !$omp teams distribute simd num_teams(dims(2):3,3)
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"20\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(1458\\) thread_limit\\(938\\) num_teams\\(dims\\(\\):3, 3\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(dims\\(\\):3, 3\\)" 1 "original" } }

 !$omp target thread_limit(729) dyn_groupprivate( 803) message("21")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 !$omp teams distribute num_teams(4223)
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"21\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(803\\) thread_limit\\(729\\) num_teams\\(4223\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(4223\\)" 1 "original" } }

 !$omp target thread_limit(451) dyn_groupprivate( 842) message("22")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 !$omp teams loop num_teams(444)
 do i = 1, n
 end do
 !$omp end target
! { dg-final { scan-tree-dump-times "#pragma omp target message\\(\"22\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(842\\) thread_limit\\(451\\) num_teams\\(444\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams num_teams\\(444\\)" 1 "original" } }


 ! No 'target', just teams

 !$omp teams distribute parallel do simd num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("23") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(\"23\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) num_teams\\(11:22\\) thread_limit\\(strict:331\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"23\" \\\[len:2\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp teams distribute parallel do num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("24") severity(warning)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(\"24\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) num_teams\\(11:22\\) thread_limit\\(strict:331\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel message\\(\"24\" \\\[len:2\\\]\\) severity\\(warning\\)" 1 "original" } }

 !$omp teams distribute simd num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("25")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(\"25\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) num_teams\\(11:22\\) thread_limit\\(strict:331\\)" 1 "original" } }

 !$omp teams distribute num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("26")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(\"26\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) num_teams\\(11:22\\) thread_limit\\(strict:331\\)" 1 "original" } }

 !$omp teams loop num_teams(11:22) thread_limit(strict : 331) dyn_groupprivate( cgroup ,fallback(null) : 243) message("27")  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
 do i = 1, n
 end do
! { dg-final { scan-tree-dump-times "#pragma omp teams message\\(\"27\" \\\[len:2\\\]\\) severity\\(fatal\\) dyn_groupprivate\\(fallback\\(null\\):243\\) num_teams\\(11:22\\) thread_limit\\(strict:331\\)" 1 "original" } }
end
