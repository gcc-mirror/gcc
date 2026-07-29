/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

float a[256], b[256], c[256], d[256], r[256];

__attribute__((noipa)) void
f (int n)
{
  for (int i = 0; i < n; i++)
    {
      float t = d[i] > 0.f ? __builtin_fmaf (-a[i], b[i], c[i]) : d[i];
      r[i] = -t;
    }
}

int
main (void)
{
  for (int i = 0; i < 256; i++)
    {
      a[i] = 2.f;
      b[i] = 3.f;
      c[i] = 5.f;
      d[i] = i & 1 ? 4.f : -7.f;
      r[i] = 0.f;
    }

  f (256);
  for (int i = 0; i < 256; i++)
    {
      float expected = d[i] > 0.f ? 1.f : 7.f;
      if (r[i] != expected)
	__builtin_abort ();
    }

  return 0;
}
