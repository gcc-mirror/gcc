/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */
/* { dg-additional-options "-std=c++11 -O3 -fopt-info-vec-optimized -fdump-tree-original -fdump-tree-gimple" } */

/* PR other/33426 */
/* Testing whether #pragma ivdep is working.  */

int ar[100];

void foo(int *a) {
#pragma GCC ivdep
  for (auto &i : ar) {
    i *= *a;
  }
}

/* { dg-message "loop vectorized" "" { target *-*-* } 0 } */
/* { dg-bogus " version\[^\n\r]* alias" "" { target *-*-* } 0 } */

/* { dg-final { scan-tree-dump-times "ANNOTATE_EXPR " 1 "original" } } */
/* { dg-final { scan-tree-dump-times "ANNOTATE " 1 "gimple" } } */
