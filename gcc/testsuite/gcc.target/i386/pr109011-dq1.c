/* { dg-do compile } */
/* { dg-options "-march=icelake-server -O3" } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \t\]+"  5 } } */

void
popcntd (unsigned int *p, unsigned int *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcountll (q[i]);
}

void
clzd (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clzll (q[i]);
}

void
ffsd (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffsll (q[i]);
}

void
ctzd (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ctzll (q[i]);
}

void
clzd0 (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clzll (q[i]) : 32;
}

void
ctzd0 (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctzll (q[i]) : 32;
}
