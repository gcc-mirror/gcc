/* { dg-do compile } */
/* { dg-options "-Wno-aggressive-loop-optimizations" } */

int a, b;
struct S { unsigned int s, t, u; } c, d = { 0, 1, 0 };

void
test (unsigned char z)
{
  char e[] = {0, 0, 0, 0, 1};
  for (c.s = 1; c.s; c.s++)
    {
      b = e[c.s];
      if (a)
	break;
      b = z >= c.u;
      if (d.t)
	break;
    }
}
