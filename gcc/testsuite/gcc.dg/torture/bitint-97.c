/* C2Y N3747 - Integer Sets, v5 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c2y -pedantic-errors -fwrapv" } */

[[gnu::noipa]] void
bar (_BitInt(1) a[8], signed _BitInt(1) b[8], const _BitInt(1) c[8])
{
  a[0] = b[0] + c[0];
  a[1] = b[1] - c[1];
  a[2] = b[2] / c[2];
  a[3] = ++b[3];
  a[4] = b[4]++;
  a[5] = --b[5];
  a[6] = b[6]--;
  a[7] = -b[7];
}

int
main ()
{
  _BitInt(1) e[8];
  _BitInt(1) f[8] = {
    -1, 0, -1, 0, 0, -1, -1, -1,
  };
  signed _BitInt(1) g[8] = {
    -1, -1, -1, -1, -1, 0, 0, -1,
  };
  bar (e, f, g);
  _BitInt(1) h[8] = {
    0, -1, -1, -1, 0, 0, -1, -1
  };
  for (int i = 0; i < 8; ++i)
    if (e[i] != h[i] || (i > 2 && f[i] != g[i]))
      __builtin_abort ();
}
