/* PR tree-optimization/83957 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-parallelize-loops=2 -fno-tree-dce --param parloops-schedule=dynamic" } */

void
foo (int *x, int y)
{
  if (y < 0)
    for (; y < 1; ++y)
      x = &y;
}
