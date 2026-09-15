/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_ok } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */
/* { dg-additional-options "-O3 -fno-unroll-loops" } */

#pragma GCC target "+nosve"

#define N 8

unsigned char upix1[N], upix2[N];
signed char spix1[N], spix2[N];

int
ufoo (void)
{
  int sum = 0;
  int i;
  for (i = 0; i < N; ++i)
    sum += __builtin_abs (upix1[i] - upix2[i]);
  return sum;
}

int
sfoo (void)
{
  int sum = 0;
  int i;
  for (i = 0; i < N; ++i)
    sum += __builtin_abs (spix1[i] - spix2[i]);
  return sum;
}

/* { dg-final { scan-assembler {\tuabd\tv[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b} } } */
/* { dg-final { scan-assembler {\tsabd\tv[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b} } } */
/* { dg-final { scan-assembler-times {\tudot\tv[0-9]+\.2s, v[0-9]+\.8b, v[0-9]+\.8b} 2 } } */
