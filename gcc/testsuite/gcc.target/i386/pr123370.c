/* PR c/123370 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" } */

float x, y;

_Static_assert (_Generic (!(x - y), int: 1, default: 0), "");

int
f1 (int c)
{
  return c ? (y + 1) : !(x - y);
}

int
f2 (int c)
{
  return c ? !(x - y) : !!(x - y);
}

float
f3 (void)
{
  return !(x - y) / 2;
}
