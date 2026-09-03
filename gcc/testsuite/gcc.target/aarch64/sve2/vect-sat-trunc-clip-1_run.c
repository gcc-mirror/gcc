/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve2_hw } */
/* { dg-options "-O3 -mautovec-preference=sve-only" } */

typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;

/* Not a multiple of any SVE vector length, so that the loop runs a
   predicated tail iteration.  */
#define N 257

/* Clipping to [0, 65535] written so that the negation cannot overflow.  At
   INT_MIN the negation is its own inverse, so the shift yields -1 and the
   result is 65535 rather than 0.  */

void __attribute__((noipa))
clip (uint16_t *__restrict out, const int *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int x = in[i];
      out[i] = ((uint32_t) x > 65535u
		? (int) (-(uint32_t) x) >> 31
		: x);
    }
}

int
main (void)
{
  int in[N];
  uint16_t out[N];

  for (int i = 0; i < N; ++i)
    in[i] = (i & 3) == 0 ? (-__INT_MAX__ - 1) : i * 12345 - 30000;

  clip (out, in, N);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      int x = in[i];
      uint16_t ref = ((uint32_t) x > 65535u
		      ? (int) (-(uint32_t) x) >> 31
		      : x);
      if (out[i] != ref)
	__builtin_abort ();
    }

  return 0;
}
