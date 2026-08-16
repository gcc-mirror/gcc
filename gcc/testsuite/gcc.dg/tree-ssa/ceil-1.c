/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* PR tree-optimization/126827 */

double f(double a)
{
    if (a > 0)
    {
      a = __builtin_ceil(a);
      if (a < 0)
        __builtin_trap();
    }
    return a;
}

/* The call to __builtin_trap should have been removed.  */
/* { dg-final { scan-tree-dump-not "trap " "optimized" } } */
