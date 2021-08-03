// { dg-do compile }
// { dg-options "-O2 -march=haswell" }

#include <cstring>

int *
foo()
{
  int * p = new int[16];
  memset(p,0,16*sizeof(int));
  return p;
}

/* { dg-final { scan-assembler-times "vpxor\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 2 } } */
