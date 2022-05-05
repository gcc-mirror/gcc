/* { dg-do run { target avx512f } } */
/* { dg-require-effective-target sse2_runtime } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */

#include <x86intrin.h>

union U {
  float m[4][4];
  __m128 r[4];
  __m512 s;
};

__attribute__((noipa, target("avx512f")))
void
foo (union U *x, union U *a, union U *b)
{
  __m512 c = _mm512_loadu_ps (&a->s);
  __m512 d = _mm512_broadcast_f32x4 (b->r[0]);
  __m512 e = _mm512_broadcast_f32x4 (b->r[1]);
  __m512 f = _mm512_broadcast_f32x4 (b->r[2]);
  __m512 g = _mm512_broadcast_f32x4 (b->r[3]);
  __m512 h = _mm512_mul_ps (_mm512_permute_ps (c, 0x00), d);
  h = _mm512_fmadd_ps (_mm512_permute_ps (c, 0x55), e, h);
  h = _mm512_fmadd_ps (_mm512_permute_ps (c, 0xaa), f, h);
  h = _mm512_fmadd_ps (_mm512_permute_ps (c, 0xff), g, h);
  _mm512_storeu_ps (&x->s, h);
}

__attribute__((noipa, target("avx512f")))
void
do_test (void)
{
  union U a = { .m = { { 1.0f, 2.0f, 3.0f, 4.0f },
		       { 5.0f, 6.0f, 7.0f, 8.0f },
		       { 9.0f, 10.0f, 11.0f, 12.0f },
		       { 13.0f, 14.0f, 15.0f, 16.0f } } };
  union U b = { .m = { { 17.0f, 18.0f, 19.0f, 20.0f },
		       { 21.0f, 22.0f, 23.0f, 24.0f },
		       { 25.0f, 26.0f, 27.0f, 28.0f },
		       { 29.0f, 30.0f, 31.0f, 32.0f } } };
  union U c;
  foo (&c, &a, &b);
  if (c.m[0][0] != 250.0f
      || c.m[0][1] != 260.0f
      || c.m[0][2] != 270.0f
      || c.m[0][3] != 280.0f)
    __builtin_abort ();
  if (c.m[1][0] != 618.0f
      || c.m[1][1] != 644.0f
      || c.m[1][2] != 670.0f
      || c.m[1][3] != 696.0f)
    __builtin_abort ();
  if (c.m[2][0] != 986.0f
      || c.m[2][1] != 1028.0f
      || c.m[2][2] != 1070.0f
      || c.m[2][3] != 1112.0f)
    __builtin_abort ();
  if (c.m[3][0] != 1354.0f
      || c.m[3][1] != 1412.0f
      || c.m[3][2] != 1470.0f
      || c.m[3][3] != 1528.0f)
    __builtin_abort ();
}

int
main ()
{
  if (__builtin_cpu_supports ("avx512f"))
    do_test ();
  return 0;
}
