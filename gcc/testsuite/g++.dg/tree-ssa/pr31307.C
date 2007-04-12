/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

union MY_M128
{
  double i;
};

struct RegFile
{
  MY_M128 dst[4];
};

__inline__ __attribute__((always_inline)) static void
MEM_OPT_LOAD(MY_M128* reg, double* mem)
{
  reg[0].i = *mem;
}

void _ia32_movntdq (double *, double);

__inline__ __attribute__((always_inline)) static void
MEM_OPT_STORE(MY_M128* reg, double* mem)
{
  _ia32_movntdq ((double*)mem, (double)reg[0].i);
}

double _mm_adds_epu8 (double __A, double __B);

int test(unsigned char *d)
{ 
  RegFile r;
  MEM_OPT_LOAD((r.dst) , ((double*) d)); 
  r.dst[0].i = _mm_adds_epu8(r.dst[0].i, r.dst[0].i);
  MEM_OPT_STORE((r.dst), (double*) d);
  return 0;
}

/* { dg-final { scan-tree-dump-not "r.dst" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
