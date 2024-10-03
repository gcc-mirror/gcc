/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8 

int
main1 ()
{
  int i;
  unsigned short in[N*8] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
  unsigned short out[N*8], a[N], b[N] = {3,0x8031,0x7fff,0x8032,0xffff,0,0x8030,0x8000};

  /* Partial SLP is not supported.  */
  for (i = 0; i < N; i++)
    {
      out[i*4] = in[i*4];
      out[i*4 + 1] = in[i*4 + 1];
      out[i*4 + 2] = in[i*4 + 2];
      out[i*4 + 3] = in[i*4 + 3];

      a[i] = b[i] / 0x8031;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (out[i*4] !=  in[i*4]
         || out[i*4 + 1] != in[i*4 + 1]
         || out[i*4 + 2] != in[i*4 + 2]
         || out[i*4 + 3] != in[i*4 + 3]
	 || a[i] != b[i] / 0x8031)
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { target { ! { mips_msa || { amdgcn-*-* || { riscv_v || loongarch_sx } } } } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { mips_msa || { amdgcn-*-* || { riscv_v || loongarch_sx } } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target { ! { mips_msa || { amdgcn-*-* || { riscv_v || loongarch_sx } } } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { mips_msa || loongarch_sx } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target { riscv_v || amdgcn-*-* } } } } */
