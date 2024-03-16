/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int *q, *r, *s;

void
foo (int *p)
{
  p[4wb] = 0;
  6wb[p] = 0;
  q = p + 8wb;
  r = 10uwb + q;
  s = r - 2wb;
}
