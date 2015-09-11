/* PR tree-optimization/46909 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern void abort ();

int
__attribute__ ((__noinline__))
foo (unsigned int x)
{
  if (! (x == 4 || x == 6) || (x == 2 || x == 6))
    return 1;
  return -1;
}

/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) != 4" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) != 6" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) == 2" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9\]+\\(D\\) == 6" 0 "optimized" } } */
