/* PR tree-optimization/81578 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-O2 -ftree-parallelize-loops=2" } */

int
foo (int *x)
{
  int i, r = 1;
  for (i = 0; i != 1024; i++)
    r *= x[i] < 0;
  return r;
}
