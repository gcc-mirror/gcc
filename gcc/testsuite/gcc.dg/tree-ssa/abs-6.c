/* { dg-do compile } */
/* { dg-options "-O2 -fno-signed-zeros -fdump-tree-optimized" } */

/* Trapping math stays on.  A float parameter can be a NaN, so the
   ordered compare must not become ABS.  A conversion from an integer
   cannot be a NaN, so that compare can.  */

float keep_cmp (float a) { return a < 0 ? -a : a; }

float from_int (int i) { return (float)i < 0 ? -(float)i : (float)i; }

/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump " < " "optimized" } } */
