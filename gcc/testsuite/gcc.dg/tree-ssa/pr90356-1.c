/* PR tree-optimization/90356 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-rounding-math -fsignaling-nans -fsigned-zeros -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]*.D. \\+ 0.0;" 12 "optimized" } } */
/* { dg-final { scan-tree-dump-times "y_\[0-9]*.D. - 0.0;" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \[+-] 0.0;" 16 "optimized" } } */

double f1 (double x) { return (x + 0.0) + 0.0; }
double f2 (double y) { return (y + (-0.0)) + (-0.0); }
double f3 (double y) { return (y - 0.0) - 0.0; }
double f4 (double x) { return (x - (-0.0)) - (-0.0); }
double f5 (double x) { return (x + 0.0) - 0.0; }
double f6 (double x) { return (x + (-0.0)) - (-0.0); }
double f7 (double x) { return (x - 0.0) + 0.0; }
double f8 (double x) { return (x - (-0.0)) + (-0.0); }
double f9 (double x) { double t = x + 0.0; return t + 0.0; }
double f10 (double y) { double t = y + (-0.0); return t + (-0.0); }
double f11 (double y) { double t = y - 0.0; return t - 0.0; }
double f12 (double x) { double t = x - (-0.0); return t - (-0.0); }
double f13 (double x) { double t = x + 0.0; return t - 0.0; }
double f14 (double x) { double t = x + (-0.0); return t - (-0.0); }
double f15 (double x) { double t = x - 0.0; return t + 0.0; }
double f16 (double x) { double t = x - (-0.0); return t + (-0.0); }
