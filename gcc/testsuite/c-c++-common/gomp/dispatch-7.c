/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

int f0 (void);
int f1 (void);
#pragma omp declare variant (f0) match (construct={dispatch})
#pragma omp declare variant (f1) match (implementation={vendor(gnu)})
int f2 (void);

int test (void)
{
  int a, n;
#pragma omp dispatch novariants(n < 1024) nocontext(n > 1024)
  a = f2 ();
  return a;
}

/* { dg-final { scan-tree-dump-times "#pragma omp dispatch nocontext\\(0\\) novariants\\(0\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "a = f2 \\\(\\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "a = f1 \\\(\\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "a = f0 \\\(\\\);" 1 "gimple" } } */
