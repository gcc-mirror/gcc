/* { dg-options "-O3 --param vect-partial-vector-usage=1" } */

void __attribute__((noipa))
add_loop (unsigned int *x, int n, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < n; ++i)
    {
      res0 += x[i * 2];
      res1 += x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
min_loop (unsigned int *x, int n, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < n; ++i)
    {
      res0 = res0 < x[i * 2] ? res0 : x[i * 2];
      res1 = res1 < x[i * 2 + 1] ? res1 : x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
max_loop (unsigned int *x, int n, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < n; ++i)
    {
      res0 = res0 > x[i * 2] ? res0 : x[i * 2];
      res1 = res1 > x[i * 2 + 1] ? res1 : x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
and_loop (unsigned int *x, int n, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < n; ++i)
    {
      res0 &= x[i * 2];
      res1 &= x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
or_loop (unsigned int *x, int n, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < n; ++i)
    {
      res0 |= x[i * 2];
      res1 |= x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
eor_loop (unsigned int *x, int n, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < n; ++i)
    {
      res0 ^= x[i * 2];
      res1 ^= x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuaddv\t} 2 } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuminv\t} 2 } } */

/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumaxv\t} 2 } } */

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tandv\t} 2 } } */

/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\torv\t} 2 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\teorv\t} 2 } } */
