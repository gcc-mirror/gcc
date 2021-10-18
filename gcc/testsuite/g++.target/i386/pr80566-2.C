// { dg-do compile }
// { dg-options "-O2 -march=haswell -mtune-ctrl=avx256_move_by_pieces" }

#include <cstring>

int *
foo(int * q)
{
  int * p = new int[16];
  memcpy(q,p,16*sizeof(int));
  return p;
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 4 } } */
