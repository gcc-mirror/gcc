/* PR c/123370 */
/* { dg-do run } */
/* { dg-options "-O2 -fexcess-precision=standard" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16_runtime } */

volatile _Float16 x = 3, y = 1;

_Static_assert (_Generic (!(x - y), int: 1, default: 0), "");
_Static_assert (sizeof (!(x - y)) == sizeof (int), "");

__attribute__((noipa)) int
f1 (void)
{
  return (y == x) ? (y + 1) : !(x - y);
}

__attribute__((noipa)) int
f2 (int c)
{
  return c ? !(x - y) : !!(x - y);
}

__attribute__((noipa)) float
f3 (void)
{
  return !(x - x) / 2;
}

int
main (void)
{
  if (f1 () != 0 || f2 (1) != 0 || f2 (0) != 1 || f3 () != 0)
    __builtin_abort ();
  return 0;
}
