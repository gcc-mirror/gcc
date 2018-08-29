/* PR target/85317 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */

#include <x86intrin.h>

extern void link_error (void);

int
main ()
{
  int a = _mm_movemask_pd (_mm_set_pd (-2.0, 2.0));
  if (a != 2) link_error ();
  int b = _mm_movemask_pd (_mm_set_pd (0.0, __builtin_copysign (0.0, -4.0)));
  if (b != 1) link_error ();
  int c = _mm_movemask_ps (_mm_set_ps (__builtin_copysignf (0.0f, -4.0f), 0.0f,
				       -4.0f, 4.0f));
  if (c != 10) link_error ();
  int d = _mm_movemask_epi8 (_mm_set_epi8 (-4, 8, -8, -12, 12, 15, 0, -1, -3,
					   -128, 127, 126, 120, -120, 0, 5));
  if (d != 0xb1c4) link_error ();
  int e = _mm256_movemask_pd (_mm256_set_pd (-4.0, 0.0, 4.0,
					     __builtin_copysign (0.0, -4.0)));
  if (e != 9) link_error ();
  int f = _mm256_movemask_ps (_mm256_set_ps (-8.0f, -16.0f, 12.0f, 127.0f,
					     -4.0f, 0.0f, 4.0f,
					     __builtin_copysign (0.0f,
								 -4.0f)));
  if (f != 0xc9) link_error ();
  int g = _mm256_movemask_epi8 (_mm256_set_epi8 (-4, 8, -8, -12, 12, 15, 0, -1,
						 -3, -128, 127, 126, 120, -120,
						 0, 5, 12, 100, -20, -50, -70,
						 2, -65, 0, -1, 1, 2, -2, -9,
						 -9, 19, -64));
  if (g != (int) 0xb1c43a9dU) link_error ();
  return 0;
}
