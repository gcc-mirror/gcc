#include CHECK_H

__m128i
__attribute__((noinline))
foo (short x1, short x2, short x3, short x4,
     short x5, short x6, short x7, short x8)
{
  return _mm_set_epi16 (x1, x2, x3, x4, x5, x6, x7, x8);
}

static void
TEST (void)
{
  short v[8] = { -3, 2, 1, 9, 23, -173, -13, 69 };
  union128i_w u;

  u.x = foo (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union128i_w (u, v))
     abort ();
}
