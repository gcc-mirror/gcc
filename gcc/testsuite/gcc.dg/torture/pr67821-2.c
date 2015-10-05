/* { dg-do compile } */

int a, b, c, d, e, g;
short f;

void
fn1 ()
{
  int i;
  f = a - b;
  e = (c && (i = d = (unsigned) f - 1)) || i;
  g = (unsigned) f - 1;
  c && (d = 0);
}
