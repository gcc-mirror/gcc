/* PR rtl-optimization/85300 */
/* { dg-do compile } */
/* { dg-options "-O1 -g -funroll-all-loops -fno-tree-ter -fno-web" } */

void
foo (double x, unsigned char y)
{
  while ((int) x < 1)
    {
      float a;

      a = y | 0x100;
      y = 0;
      x = a;
    }
}
