/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include CHECK_H

static __m128i
__attribute__((noinline))
foo (char *v)
{
  return _mm_set_epi8 (v[15], v[14], v[13], v[12],
		       v[11], v[10], v[9], v[8],
		       v[7], v[6], v[5], v[4],
		       v[3], v[2], v[1], v[0]);
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

  u.x = foo (v);
  if (check_union128i_b (u, v))
    abort ();
}
