/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -fopt-info-vec-optimized" } */

/* PR other/33426 */
/* Testing whether #pragma ivdep is working.  */

void foo(int n, int *a, int *b, int *c, int *d, int *e) {
  int i;
#pragma GCC ivdep
  for (i = 0; i < n; ++i) {
    a[i] = b[i] + c[i];
  }
}

/* { dg-message "loop vectorized" "" { target *-*-* } 0 } */
/* { dg-bogus " version" "" { target *-*-* } 0 } */
/* { dg-bogus " alias" "" { target *-*-* } 0 } */
/* { dg-final { cleanup-tree-dump "vect" } } */
