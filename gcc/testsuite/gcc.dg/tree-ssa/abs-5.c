/* { dg-do compile } */
/* { dg-options "-O2 -fno-signed-zeros -fno-trapping-math -fdump-tree-optimized" } */

/* Fold, when signed zeros need not be preserved:
     A </<= 0 ? -A : A  ->  ABS (A)
     A >/>= 0 ? -A : A  ->  -ABS (A)

   ABS does not trap.  With NaNs honored, -fno-trapping-math is required
   so the ordered compare's invalid exception is not observable.  The
   compare is matched directly: its inverse is UNGE.  Both arms are NaN
   when A is NaN.  */

float abs_lt (float a) { return a < 0 ? -a : a; }
float abs_le (float a) { return a <= 0 ? -a : a; }
float abs_if (float a) { if (a < 0) a = -a; return a; }
double abs_lt_d (double a) { return a < 0 ? -a : a; }

float nabs_gt (float a) { return a > 0 ? -a : a; }
float nabs_ge (float a) { return a >= 0 ? -a : a; }

/* -ABS becomes .COPYSIGN (x, -1) when the target has the copysign optab.  */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 4 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "= \\.COPYSIGN" 2 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 6 "optimized" { target { ! ifn_copysign } } } } */
/* { dg-final { scan-tree-dump-not " < " "optimized" } } */
/* { dg-final { scan-tree-dump-not " <= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " > " "optimized" } } */
/* { dg-final { scan-tree-dump-not " >= " "optimized" } } */
