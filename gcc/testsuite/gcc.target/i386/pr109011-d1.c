/* { dg-do compile } */
/* { dg-options "-march=icelake-server -O3" } */
/* { dg-final { scan-assembler-times "vpopcntd\[ \t\]+" 1 } } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \t\]+"  5 } } */

void
popcntd (unsigned int *p, unsigned int *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcount (q[i]);
}

void
clzd (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clz (q[i]);
}

void
ffsd (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffs (q[i]);
}

void
ctzd (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ctz (q[i]);
}

void
clzd0 (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clz (q[i]) : 32;
}

void
ctzd0 (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctz (q[i]) : 32;
}
