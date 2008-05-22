#include CHECK_H

static __m128i
__attribute__((noinline))
foo (char x, int i)
{
  switch (i)
    {
    case 15:
      return _mm_set_epi8 (x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 14:
      return _mm_set_epi8 (1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 13:
      return _mm_set_epi8 (1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 12:
      return _mm_set_epi8 (1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 11:
      return _mm_set_epi8 (1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 10:
      return _mm_set_epi8 (1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 9:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    case 8:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1, 1);
    case 7:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1, 1);
    case 6:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1, 1);
    case 5:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1, 1);
    case 4:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1, 1);
    case 3:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1, 1);
    case 2:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1, 1);
    case 1:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, 1);
    case 0:
      return _mm_set_epi8 (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x);
    default:
      abort ();
    }
}

static void
TEST (void)
{
  char e = 0x13;
  char v[16];
  union128i_b u;
  int i, j;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    {
      for (j = 0; j < ARRAY_SIZE (v); j++)
	v[j] = 1;
      v[i] = e;
      u.x = foo (e, i);
      if (check_union128i_b (u, v))
	abort ();
    }
}
