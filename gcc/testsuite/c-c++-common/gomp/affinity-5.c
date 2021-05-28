/* { dg-additional-options "-fdump-tree-gimple" } */
int bar (int);
int bar2 (int);

void foobar()
{
  int d[64], e[64], f[64];
#pragma omp task affinity (d, e[bar(5)], f[4:10])
  ;
}

void
foo (void)
{
  int a[64];
#pragma omp task affinity (iterator (j=bar(0):bar(1):bar(2))  : a[bar(j)])
  ;
}
void
qux (void)
{
  int a[64], b[64], c[64];
#pragma omp task affinity (iterator (j=bar(0):bar(1):bar(2))  : a[bar(j+1)], b[bar(j+2)], c[bar(j+3)])
  ;
}

/* { dg-final { scan-tree-dump-times "= bar \\(5\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "= bar \\(0\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "= bar \\(1\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "= bar \\(2\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "= bar \\(j\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "= bar \\(_.\\);" 3 "gimple" } }  */
