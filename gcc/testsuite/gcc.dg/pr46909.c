/* PR tree-optimization/46909 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcombine" } */

extern void abort ();

int
__attribute__ ((__noinline__))
foo (unsigned int x)
{
  if (! (x == 4 || x == 6) || (x == 2 || x == 6))
    return 1;
  return -1;
}

/* { dg-final { scan-tree-dump "optimizing two comparisons to x_\[0-9\]+\\(D\\) != 4" "ifcombine" } } */
/* { dg-final { cleanup-tree-dump "ifcombine" } } */
