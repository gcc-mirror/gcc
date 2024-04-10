! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

module m
contains
  subroutine sub1
    !$omp declare target indirect enter (sub1)
  end subroutine
  ! { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target, omp declare target indirect\\\)\\\)\\\n.*\\\nvoid sub1" "gimple" } }

  subroutine sub2
    !$omp declare target indirect (.false.) to (sub2)
  end subroutine
  ! { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\n.*\\\nvoid sub2" "gimple" } }

  subroutine sub3
    !$omp declare target indirect (.true.) to (sub3)
  end subroutine
  ! { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target, omp declare target indirect\\\)\\\)\\\n.*\\\nvoid sub3" "gimple" } }

  subroutine sub4
    !$omp declare target indirect (.false.) enter (sub4)
  end subroutine
  ! { dg-final { scan-tree-dump "__attribute__\\\(\\\(omp declare target\\\)\\\)\\\n.*\\\nvoid sub4" "gimple" } }
end module
