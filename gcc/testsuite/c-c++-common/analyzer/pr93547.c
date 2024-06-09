/* { dg-do compile } */

void
wy (int);

int
f9 (void)
{
  int p5 = __builtin_ilogb (__builtin_inf ());

  wy (0);

  return p5;
}
