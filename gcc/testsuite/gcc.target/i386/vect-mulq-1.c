/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512dq" } */

typedef unsigned long long u64;

void
mul64_slp2 (u64 *__restrict r, u64 *a, u64 *b)
{
  r[0] = a[0] * b[0];
  r[1] = a[1] * b[1];
}

/* { dg-final { scan-assembler-times "pmullq" 1 } } */
