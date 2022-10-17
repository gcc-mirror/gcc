/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-final { scan-tree-dump "foo \\(6\\);\[\n\r]*  __sync_synchronize \\(\\);\[\n\r]*  foo \\(6\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "foo \\(4\\);\[\n\r]*  __atomic_thread_fence \\(4\\);\[\n\r]*  foo \\(4\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "foo \\(3\\);\[\n\r]*  __atomic_thread_fence \\(3\\);\[\n\r]*  foo \\(3\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "foo \\(2\\);\[\n\r]*  __atomic_thread_fence \\(2\\);\[\n\r]*  foo \\(2\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "foo \\(5\\);\[\n\r]*  __sync_synchronize \\(\\);\[\n\r]*  foo \\(5\\);" "gimple" } } */

void foo (int);

void
f1 (void)
{
  foo (4);
  #pragma omp flush acq_rel
  foo (4);
}

void
f2 (void)
{
  foo (3);
  #pragma omp flush release
  foo (3);
}

void
f3 (void)
{
  foo (2);
  #pragma omp flush acquire
  foo (2);
}

void
f4 (void)
{
  foo (5);
  #pragma omp flush
  foo (5);
}

void
f5 (void)
{
  foo (6);
  #pragma omp flush seq_cst
  foo (6);
}
