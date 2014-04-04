/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a" } */


void foo (void);

void
bar (int x, int y)
{
  y = 9999;
  if (x & (1 << y))
    foo ();

