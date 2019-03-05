! Multiple OpenACC wait clauses.

! { dg-additional-options "-fdump-tree-original" } 

!$ACC PARALLEL ASYNC (1) WAIT (14)
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(1\\) wait\\(14\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (2) WAIT (11, 12) WAIT(13)
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(2\\) wait\\(11\\) wait\\(12\\) wait\\(13\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (3) WAIT
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(3\\) wait\\(-1\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (4) WAIT (100) WAIT
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(4\\) wait\\(100\\) wait\\(-1\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (5) WAIT WAIT (101)
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(5\\) wait\\(-1\\) wait\\(101\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (6) WAIT WAIT (102, 103) WAIT WAIT
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(6\\) wait\\(-1\\) wait\\(102\\) wait\\(103\\) wait\\(-1\\) wait\\(-1\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (7) WAIT (104) WAIT WAIT (105, 106)
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(7\\) wait\\(104\\) wait\\(-1\\) wait\\(105\\) wait\\(106\\)$" 1 "original" } }

      END
