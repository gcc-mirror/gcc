/* PR rtl-optimization/98439 */
/* { dg-do compile } */
/* { dg-options "-march=nehalem -O2 -fselective-scheduling2 -fno-cprop-registers" } */

int v;
int bar (int, int, int, int, int, int, int);

int
foo (void)
{
  return bar (0, 0, 0, 0, 0, 0, v > 0 ? v : 0);
}
