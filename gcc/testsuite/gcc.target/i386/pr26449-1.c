/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mtune=k8" } */
/* { dg-require-effective-target sse2 } */

typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16)));

void sse2_test (void)
{
  union
  {
    __m128i x;
  } val1, res[8], tmp;
  short ins[8] = { 8, 5, 9, 4, 2, 6, 1, 20 };
  int i;

  for (i = 0; i < 8; i++)
    {
      tmp.x = val1.x;
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
        (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi) val1.x, ins[i], 0);
    }
}
