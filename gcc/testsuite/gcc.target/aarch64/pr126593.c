/* { dg-do run { target arm_v8_3a_complex_neon_hw } } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-O3 -march=armv8.3-a -ffast-math" } */

#define N 64
_Complex float a[N], b[N], d[N], c[N];

__attribute__((noipa))
void
mix (int n)
{
  for (int i = 0; i < n; i++)
    {
      __real__ c[i] = __real__ a[i] * __real__ b[i] - __imag__ a[i] * __imag__ d[i];
      __imag__ c[i] = __real__ a[i] * __imag__ b[i] + __imag__ a[i] * __real__ d[i];
    }
}

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = 1.0f + 2.0fi;
      b[i] = 3.0f + 4.0fi;
      d[i] = 5.0f + 6.0fi;
    }

  mix (N);

  /* scalar: re = 1*3 - 2*6 = -9, im = 1*4 + 2*5 = 14.
     vectorized as a*b: re = -5, im = 10.  */
  if (__real__ c[0] != -9.0f || __imag__ c[0] != 14.0f)
    __builtin_abort ();

  return 0;
}
