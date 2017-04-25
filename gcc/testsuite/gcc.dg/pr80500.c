/* PR rtl-optimization/80500 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -ftree-loop-if-convert -fvariable-expansion-in-unroller" } */

signed char v;

void
foo (int x)
{
  while (x != 0)
    {
      v = (x >= 0) + 1;
      ++x;
    }
}
