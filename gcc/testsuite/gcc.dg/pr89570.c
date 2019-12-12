/* PR tree-optimization/89570 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize -fno-trapping-math -fno-tree-dce -fno-tree-dominator-opts" } */
/* { dg-additional-options "-mvsx" { target powerpc_vsx_ok } } */

void
foo (double *x, double *y, double *z)
{
  int i;
  for (i = 0; i < 7; i += 2)
    {
      x[i] = y[i] ? z[i] / 2.0 : z[i];
      x[i + 1] = y[i + 1] ? z[i + 1] / 2.0 : z[i + 1];
    }
}
