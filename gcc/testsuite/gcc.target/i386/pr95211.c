/* PR target/95211 target/95256 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ftree-slp-vectorize -march=skylake-avx512" } */

extern float f[4];
extern long long l[2];
extern long long ul[2];

void
fix_128 (void)
{
  l[0] = f[0];
  l[1] = f[1];
}

void
fixuns_128 (void)
{
  ul[0] = f[0];
  ul[1] = f[1];
}

void
float_128 (void)
{
  f[0] = l[0];
  f[1] = l[1];
}

void
floatuns_128 (void)
{
  f[0] = ul[0];
  f[1] = ul[1];
}

/* { dg-final { scan-assembler-times "vcvttps2qq" 2 } } */
/* { dg-final { scan-assembler-times "vcvtqq2ps" 2 } } */
