/* With trapping-math enabled (default behavior), GCC must preserve the +0.0 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

double f(double a)
{
  return __builtin_fabs(a + 0.0);
}
/* { dg-final { scan-tree-dump-times "\\+ 0\\.0" 1 "optimized" } } */
