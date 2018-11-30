! Multiple OpenACC wait clauses.

! { dg-additional-options "-fdump-tree-original" } 

!$ACC PARALLEL ASYNC (1) WAIT (14)
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(1\\) wait\\(14\\)$" 1 "original" } }

!$ACC PARALLEL ASYNC (2) WAIT (11, 12) WAIT(13)
!$ACC END PARALLEL
! { dg-final { scan-tree-dump-times "(?n)#pragma acc parallel async\\(2\\) wait\\(11\\) wait\\(12\\) wait\\(13\\)$" 1 "original" } }

      END
