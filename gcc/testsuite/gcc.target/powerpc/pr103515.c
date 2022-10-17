/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-loop2_unroll-optimized" } */

/* The pragma specified for foo2 should not affect foo1.
   Verify compiler won't perform unrolling for foo1.  */

#define N 1024
extern int a1[N], b1[N], c1[N];
extern int a2[N], b2[N], c2[N];
extern int n;

void
foo1 ()
{
  int i;
  for (i = 0; i < n; i++)
    c1[i] += a1[i] + b1[i];
}

#pragma GCC optimize("O3,unroll-loops")
void
foo2 ()
{
  int i;
  for (i = 0; i < n; i++)
    c2[i] += a2[i] + b2[i];
}

/* { dg-final { scan-rtl-dump-times "optimized: loop unrolled" 1 "loop2_unroll" } } */

