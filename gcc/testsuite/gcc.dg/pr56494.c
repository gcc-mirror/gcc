/* PR rtl-optimization/56494 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftracer -w" } */

char a;
short b;
void bar (int);

void
foo (void)
{
  bar ((!!b ? : (a *= a / 0)) >= (a = b));
}
