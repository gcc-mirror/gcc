/* { dg-do compile } */
/* { dg-options "-O2 -fassociative-math -fno-signed-zeros -fno-trapping-math -ffinite-math-only -fdump-tree-optimized" } */

/* Reassociation permits the sum and subtraction to cancel for floating
   point MIN_EXPR and MAX_EXPR.  */

float f1 (float a, float b) { float m = a < b ? a : b; return (a + b) - m; }
float f2 (float a, float b) { float m = a < b ? b : a; return (a + b) - m; }
double f3 (double a, double b) { double m = a < b ? a : b; return (a + b) - m; }
double f4 (double a, double b) { double m = a < b ? b : a; return (a + b) - m; }

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\+ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " - " "optimized" } } */
