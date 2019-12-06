/* PR rtl-optimization/92510 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-vectorize -fno-forward-propagate -fno-tree-scev-cprop" } */

int v;

long int
foo (long int x)
{
  signed char i;

  for (i = 0; i < 8; ++i)
    x |= !!v;

  return x + i;
}
