/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Strict floating-point arithmetic does not permit reassociation.  */

float f1 (float a, float b) { float m = a < b ? a : b; return (a + b) - m; }
double f2 (double a, double b) { double m = a < b ? b : a; return (a + b) - m; }

/* { dg-final { scan-tree-dump-times " \\+ " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " - " 2 "optimized" } } */
