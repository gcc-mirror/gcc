/* { dg-do compile } */
/* { dg-options "-march=icelake-server -O3" } */
/* { dg-final { scan-assembler-times "vpopcntw\[ \t\]+" 4 } } */
/* 2 vplzcntd come from function clzw, the other 2 come from function clzb0.  */
/* { dg-final { scan-assembler-times "vplzcntd\[ \t\]+" 4 } } */

void
popcntw (unsigned short *p, unsigned short *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcount (q[i]);
}

void
clzw (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clz (q[i]);
}

void
ffsw (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffs (q[i]);
}

void
ctzw (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ctz (q[i]);
}

void
clzw0 (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clz (q[i]) : 16;
}

void
ctzw0 (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctz (q[i]) : 16;
}
