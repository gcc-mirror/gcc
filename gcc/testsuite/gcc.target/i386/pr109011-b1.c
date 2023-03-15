/* { dg-do compile } */
/* { dg-options "-march=icelake-server -O3" } */
/* { dg-final { scan-assembler-times "vpopcntb\[ \t\]+" 4 } } */
/* 4 vplzcntd come from function clzw, the other 4 come from function clzb0.  */
/* { dg-final { scan-assembler-times "vplzcntd\[ \t\]+" 8 } } */

void
__attribute__((noipa))
popcntb (unsigned char *p, unsigned char *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcount (q[i]);
}

void
__attribute__((noipa))
clzb (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clz (q[i]);
}

void
__attribute__((noipa))
ffsb (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffs (q[i]);
}

void
__attribute__((noipa))
ctzb (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ctz (q[i]);
}

void
__attribute__((noipa))
clzb0 (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clz (q[i]) : 8;
}

void
__attribute__((noipa))
ctzb0 (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctz (q[i]) : 8;
}
