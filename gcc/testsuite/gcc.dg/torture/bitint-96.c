/* C2Y N3747 - Integer Sets, v5 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c2y -pedantic-errors" } */

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

void
foo ()
{
  _BitInt(1) a = (_BitInt(1)) -1;
  static_assert (expr_has_type (a, signed _BitInt(1)), "");
  static_assert (expr_has_type (0wb, _BitInt(1)), "");
  static_assert (expr_has_type (~0wb, _BitInt(1)), "");
  static_assert (expr_has_type (1wb, _BitInt(2)), "");
  static_assert (expr_has_type (-1wb, _BitInt(2)), "");
  static_assert (expr_has_type (2wb, _BitInt(3)), "");
  static_assert (expr_has_type (0wb + 0wb, signed _BitInt(1)), "");
  static_assert (expr_has_type (0wb + 0uwb, unsigned _BitInt(1)), "");
  static_assert (expr_has_type (0wb + 1wb, _BitInt(2)), "");
  static_assert (expr_has_type (0uwb + 0wb, unsigned _BitInt(1)), "");
  static_assert (expr_has_type (1wb + 0wb, _BitInt(2)), "");
  static_assert (0wb == 0, "");
  static_assert (~0wb == -1, "");
  static_assert (~~0wb == 0, "");
  static_assert (-1wb == -1, "");
}

_BitInt(1) b = 0wb;
_BitInt(1) c = ~0wb;
_BitInt(1) d = 0wb;

[[gnu::noipa]] void
bar (_BitInt(1) a[21], signed _BitInt(1) b[21], const _BitInt(1) c[21])
{
  a[0] = b[0] + c[0];
  a[1] = b[1] + c[1];
  a[2] = b[2] + c[2];
  a[3] = b[3] - c[3];
  a[4] = b[4] - c[4];
  a[5] = b[5] - c[5];
  a[6] = b[6] * c[6];
  a[7] = b[7] * c[7];
  a[8] = b[8] * c[8];
  a[9] = b[9] / c[9];
  a[10] = b[10] % c[10];
  a[11] = b[10] % c[11];
  a[12] = b[12] & c[12];
  a[13] = b[13] & c[13];
  a[14] = b[14] & c[14];
  a[15] = b[15] | c[15];
  a[16] = b[16] | c[16];
  a[17] = b[17] | c[17];
  a[18] = b[18] ^ c[18];
  a[19] = b[19] ^ c[19];
  a[20] = b[20] ^ c[20];
}

#if __BITINT_MAXWIDTH__ >= 256
[[gnu::noipa]] _BitInt(256)
baz (_BitInt(256) x, _BitInt(1) y)
{
  return x * y;
}
#endif

[[gnu::noipa]] void
qux (_BitInt(1) p[3], float q, double r, long double s)
{
  p[0] = q;
  p[1] = r;
  p[2] = s;
}

[[gnu::noipa]] void
corge (_BitInt(1) p[2], float q[2], double r[2], long double s[2])
{
  q[0] = p[0];
  q[1] = p[1];
  r[0] = p[0];
  r[1] = p[1];
  s[0] = p[0];
  s[1] = p[1];
}

int
main ()
{
  if (b != 0 || c != -1)
    __builtin_abort ();
  if (0)
    {
      --b;
      ++c;
    }
  else
    {
      b += (_BitInt(1)) -1wb;
      c -= (_BitInt(1)) -1wb;
    }
  if (b != -1 || c != 0)
    __builtin_abort ();
  b = ~b;
  c = ~c;
  if (b != 0 || c != -1)
    __builtin_abort ();
  c *= d;
  if (d != 0)
    __builtin_abort ();
  _BitInt(1) e[21];
  _BitInt(1) f[21] = {
    0, -1, 0,
    0, -1, -1,
    0, -1, 0,
    0,
    0, -1,
    0, -1, -1,
    0, 0, -1,
    0, -1, -1
  };
  signed _BitInt(1) g[21] = {
    0, 0, -1,
    0, 0, -1,
    0, 0, -1,
    -1,
    -1, -1,
    0, 0, -1,
    0, -1, -1,
    0, 0, -1
  };
  bar (e, f, g);
  _BitInt(1) h[21] = {
    0, -1, -1,
    0, -1, 0,
    0, 0, 0,
    0,
    0, 0,
    0, 0, -1,
    0, -1, -1,
    0, -1, 0
  };
  for (int i = 0; i < 21; ++i)
    if (e[i] != h[i])
      __builtin_abort ();
#if __BITINT_MAXWIDTH__ >= 256
  if (baz (24807048826655379640613156875228202584070386120119745959437529938064756141700wb, ~0wb)
      != -24807048826655379640613156875228202584070386120119745959437529938064756141700wb)
    __builtin_abort ();
  if (baz (24807048826655379640613156875228202584070386120119745959437529938064756141700wb, 0wb) != 0wb)
    __builtin_abort ();
  if (baz (-24807048826655379640613156875228202584070386120119745959437529938064756141700wb, 0wb) != 0wb)
    __builtin_abort ();
  if (baz (0wb, ~0wb) != 0wb)
    __builtin_abort ();
#endif
  qux (e, 0.5f, 0.25, 0.75L);
  qux (e + 3, -0.5f, -0.25, -0.75L);
  qux (e + 6, 0.f, 0., 0.L);
  qux (e + 9, -1.f, -1., -1.L);
  qux (e + 12, -1.5f, -1.25, -1.75L);
  for (int i = 0; i < 15; ++i)
    if (e[i] != (i < 9 ? 0 : -1))
      __builtin_abort ();
  e[0] = 0wb;
  e[1] = ~0wb;
  float j[2];
  double k[2];
  long double l[2];
  corge (e, j, k, l);
  if (j[0] != 0.0f || j[1] != -1.0f
      || k[0] != 0.0 || k[1] != -1.0
      || l[0] != 0.0L || l[1] != -1.0L)
    __builtin_abort ();
}
