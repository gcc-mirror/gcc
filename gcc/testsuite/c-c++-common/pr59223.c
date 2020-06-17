/* PR c/59223 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized" } */

int foo (int x)
{
  int y;
  if (x == 0)
    y = 1;
  else if (x == 1)
    y = 2;
  return y; /* { dg-warning "may be used uninitialized" } */
}
