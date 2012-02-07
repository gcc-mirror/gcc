/* PR rtl-optimization/52060 */
/* { dg-do run { target int32plus } } */

extern void abort (void);
union U { float f; unsigned int i; };

static inline __attribute__((always_inline)) unsigned int
foo (float x)
{
  union U u;
  unsigned int a, b, c;
  int d;
  int e;
  u.f = x;
  d = ((unsigned) u.i >> 23) & 0xFF;
  c = d < 126 ? 0 : ~0;
  e = 127 + 30 - d;
  a = (u.i << 8) | 0x80000000U;
  b = a & ((1 << e) - 1);
  a = a >> e;
  c &= (b | (a & 2)) ? ~0 : ~1;
  a = ((a + 1U) >> 1) & c;
  return a;
}

__attribute__((noinline)) unsigned int
bar (float x)
{
  unsigned int a, b, c;
  static const unsigned int d[128] =
  {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 6, 7
  };
  a = foo (1048575.0f * x);
  c = d[a >> 13];
  b = (c << 13) | ((a >> (7 - c)) & 0x1fff);
  return b;
}

int
main ()
{
  union U u;
  u.f = 1048575.0f;
  if (sizeof (u.i) == sizeof (u.f)
      && u.i == 0x497ffff0U
      && bar (1.0f) != 65535)
    abort ();
  return 0;
}
