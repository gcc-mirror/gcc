! { dg-additional-options "-fdump-tree-gimple" }
! { dg-final { scan-tree-dump "foo \\(6\\);\[\n\r]*  __sync_synchronize \\(\\);\[\n\r]*  foo \\(6\\);" "gimple" } }
! { dg-final { scan-tree-dump "foo \\(4\\);\[\n\r]*  __atomic_thread_fence \\(4\\);\[\n\r]*  foo \\(4\\);" "gimple" } }
! { dg-final { scan-tree-dump "foo \\(3\\);\[\n\r]*  __atomic_thread_fence \\(3\\);\[\n\r]*  foo \\(3\\);" "gimple" } }
! { dg-final { scan-tree-dump "foo \\(2\\);\[\n\r]*  __atomic_thread_fence \\(2\\);\[\n\r]*  foo \\(2\\);" "gimple" } }
! { dg-final { scan-tree-dump "foo \\(5\\);\[\n\r]*  __sync_synchronize \\(\\);\[\n\r]*  foo \\(5\\);" "gimple" } }

module m
  interface
    subroutine foo(x)
      integer, value :: x
    end
  end interface
end module m

subroutine f1
  use m
  call foo (4)
  !$omp flush acq_rel
  call foo (4)
end

subroutine f2
  use m
  call foo (3)
  !$omp flush release
  call foo (3)
end

subroutine f3
  use m
  call foo (2)
  !$omp flush acquire
  call foo (2)
end

subroutine f4
  use m
  call foo (5)
  !$omp flush
  call foo (5)
end

subroutine f5
  use m
  call foo (6)
  !$omp flush seq_cst
  call foo (6)
end
