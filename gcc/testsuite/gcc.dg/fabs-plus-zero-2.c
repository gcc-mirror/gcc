/* With -fno-trapping-math it is safe to fold away (+/-)0.0  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-trapping-math -fdump-tree-optimized" } */

double f1(double a) { return __builtin_fabs(a + 0.0); }
double f2(double a) { return __builtin_fabs(a + -0.0); }
double f3(double a) { return __builtin_fabs(a - 0.0); }
double f4(double a) { return __builtin_fabs(a - -0.0); }

/* { dg-final { scan-tree-dump-not "\\+ 0\\.0" "optimized" } } */
