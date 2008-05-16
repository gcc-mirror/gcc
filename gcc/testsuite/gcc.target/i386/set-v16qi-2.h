#include CHECK_H

static __m128i
__attribute__((noinline))
foo (char x1, char x2, char x3, char x4,
     char x5, char x6, char x7, char x8,
     char x9, char x10, char x11, char x12,
     char x13, char x14, char x15, char x16)
{
  return _mm_set_epi8 (x1, x2, x3, x4, x5, x6, x7, x8,
		       x9, x10, x11, x12, x13, x14, x15, x16);
}

static void
TEST (void)
{
  char v[16] =
    { 
      -3, 60, 48, 104, -90, 37, -48, 78,
      4, 33, 81, 4, -89, 17, 8, 68
    };
  union128i_b u;

  u.x = foo (v[15], v[14], v[13], v[12],
	     v[11], v[10], v[9], v[8],
	     v[7], v[6], v[5], v[4],
	     v[3], v[2], v[1], v[0]);
  if (check_union128i_b (u, v))
    abort ();
}
