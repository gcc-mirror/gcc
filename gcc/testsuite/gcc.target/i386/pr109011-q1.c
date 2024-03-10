/* { dg-do compile } */
/* { dg-options "-march=icelake-server -O3" } */
/* { dg-final { scan-assembler-times "vpopcntq\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "vplzcntq\[ \t\]+"  5 } } */

void
popcntq (unsigned long long *p, unsigned long long *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcountll (q[i]);
}

void
clzq (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clzll (q[i]);
}

void
ffsq (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffsll (q[i]);
}

void
ctzq (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ctzll (q[i]);
}

void
clzq0 (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clzll (q[i]) : 64;
}

void
ctzq0 (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctzll (q[i]) : 64;
}
