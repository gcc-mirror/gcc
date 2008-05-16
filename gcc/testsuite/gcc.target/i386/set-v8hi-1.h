#include CHECK_H

static __m128i
__attribute__((noinline))
foo (short *v)
{
  return _mm_set_epi16 (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);
}

static void
TEST (void)
{
  short v[8] = { -3, 6000, 48, 104, -90, 34567, -1248, 34678 };
  union128i_w u;

  u.x = foo (v);
  if (check_union128i_w (u, v))
    abort ();
}
