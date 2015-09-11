/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int a, b, c, d;

void
fn1 ()
{
  short e;
  unsigned short g;
  
  for (c = 0; c < 1; c++)
    d = 0;
  g = ((a == 0) ^ d) % 8;
  e = g << 1;
  b = e && 1;
}
