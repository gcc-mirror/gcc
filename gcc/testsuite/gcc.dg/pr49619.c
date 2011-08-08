/* PR rtl-optimization/49619 */
/* { dg-do compile } */
/* { dg-options "-O -fno-tree-fre" } */

extern int a, b;

void
foo (int x)
{
  a = 2;
  b = 0;
  b = (a && ((a = 1, 0 >= b) || (short) (x + (b & x))));
}
