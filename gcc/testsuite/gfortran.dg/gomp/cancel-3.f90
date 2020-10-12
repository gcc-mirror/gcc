! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine foo ()
  !$omp parallel
    !$omp cancel parallel if (.true.)
    !$omp cancel parallel if (cancel: .true.)
    !$omp cancel parallel if (.false.)
    !$omp cancel parallel if (cancel: .false.)
  !$omp end parallel

  !$omp sections
    !$omp cancel sections if (cancel: .true.)
    stop
  !$omp end sections

  !$omp do
  do i = 1, 10
    !$omp cancel do if (.false.)
  end do

  !$omp task
    !$omp cancel taskgroup if (cancel: .false.)
  !$omp end task
  !$omp task
    !$omp cancel taskgroup
  !$omp end task
end subroutine

! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel \\(1, 1\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel \\(1, 0\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel \\(4, 1\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel \\(2, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel \\(8, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_cancel \\(8, 1\\);" 1 "original" } }
