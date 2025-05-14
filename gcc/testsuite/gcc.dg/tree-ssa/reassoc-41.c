/* PR tree-optimization/67815 */
/* { dg-do compile } */
/* { dg-options "-Ofast -fno-rounding-math -fdump-tree-reassoc1-details" } */

/* Test that the copysign reassoc optimization does fire for
   -fno-rounding-math (i.e. HONOR_SIGN_DEPENDENT_ROUNDING) if the multiplication
   is inexact.  */

double
f1 (double y)
{
  return (1.2 * __builtin_copysign (1.1, y));
}

double
f2 (double y)
{
  return (-1.2 * __builtin_copysign (1.1, y));
}

/* { dg-final { scan-tree-dump-times "Optimizing copysign" 2 "reassoc1" } } */
