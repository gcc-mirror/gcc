/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=m1 -fno-vect-cost-model" } */

#include "sat-trunc-promote-1.c"

#define N 256

int
main (void)
{
  int8_t input[N];
  uint8_t output[N];

  for (int i = 0; i < N; ++i)
    input[i] = i + INT8_MIN;

  /* Exercise every input value, empty loops and partial vectors.  */
  for (int n = 0; n <= N; ++n)
    {
      clip_scale_u8 (output, input, n);

#pragma GCC novector
      for (int i = 0; i < n; ++i)
	{
	  int x = input[i] * 3;
	  uint8_t expected = x < 0 ? 0 : x > UINT8_MAX ? UINT8_MAX : x;
	  if (output[i] != expected)
	    __builtin_abort ();
	}
    }

  return 0;
}
