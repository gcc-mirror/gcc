/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mprefer-vector-width=256 -mavx512f -mtune-ctrl=avx512_store_by_pieces" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 66);
}

/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
