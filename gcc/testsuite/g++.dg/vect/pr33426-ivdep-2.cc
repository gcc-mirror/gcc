/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fopt-info-vec-optimized -fdump-tree-original -fdump-tree-gimple" } */

/* PR other/33426 */
/* Testing whether #pragma ivdep is working.  */

void foo(int n, int *a, int *b, int *c) {
  int i;
 i = 0;
#pragma GCC ivdep
  while(i < n)
    {
      a[i] = b[i] + c[i];
      ++i;
    }
}

void bar(int n, int *a, int *b, int *c) {
  int i;
 i = 0;
#pragma GCC ivdep
  do
    {
      a[i] = b[i] + c[i];
      ++i;
    }
  while(i < n);
}

/* { dg-message "loop vectorized" "" { target *-*-* } 0 } */
/* { dg-bogus " version\[^\n\r]* alias" "" { target *-*-* } 0 } */

/* { dg-final { scan-tree-dump-times "ANNOTATE_EXPR " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "ANNOTATE " 2 "gimple" } } */
