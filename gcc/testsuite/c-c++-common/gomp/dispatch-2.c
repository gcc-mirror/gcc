/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

int f0 (void);
int f1 (void);
#pragma omp declare variant (f0) match (construct={dispatch})
#pragma omp declare variant (f1) match (implementation={vendor(gnu)})
int f2 (void);

int test (void)
{
  int a;
#pragma omp dispatch
  a = f2 ();
#pragma omp dispatch novariants(1)
  a = f2 ();
#pragma omp dispatch novariants(0)
  a = f2 ();
#pragma omp dispatch nocontext(1)
  a = f2 ();
#pragma omp dispatch nocontext(0)
  a = f2 ();
  return a;
}

/* { dg-final { scan-tree-dump-times "a = f0 \\\(\\\);" 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "a = f1 \\\(\\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "a = f2 \\\(\\\);" 1 "gimple" } } */
