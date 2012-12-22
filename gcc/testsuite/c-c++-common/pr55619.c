/* PR c++/55619 */
/* { dg-do compile } */

int y[4];

void
f ()
{
  int x[4] = { 0, 1, 2, 3 };
  __asm volatile ("" : : "m" (x), "m" (y));
}
