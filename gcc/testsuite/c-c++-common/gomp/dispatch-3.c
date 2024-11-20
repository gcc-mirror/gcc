/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

void f2 (void);

void test (void)
{
#pragma omp dispatch  /* { dg-final { scan-tree-dump-not "#pragma omp task" "gimple" } } */
  f2 ();
#pragma omp dispatch nowait /* { dg-final { scan-tree-dump-not "nowait" "gimple" } } */
  f2 ();
}
