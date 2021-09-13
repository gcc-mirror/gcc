/* PR tree-optimization/98455 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-dce --param case-values-threshold=1" } */

void
n4 (int io, int vb)
{
  double uc[2] = { 1.0, 2.0, };

  if (io == 0)
    uc[0] = 0.0;

  for (;;)
    if (io == 0)
      if (vb == 0)
        uc[0] = uc[1];
      else if (vb == 1)
        uc[1] = 0.0;
}
