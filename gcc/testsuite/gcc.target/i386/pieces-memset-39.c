/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mtune=generic" } */

void
foo (int a1, int a2, int a3, int a4, int a5, int a6, int x, char *dst)
{
  __builtin_memset (dst, x, 66);
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* { dg-final { scan-assembler-not "vinserti64x4" } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" { xfail *-*-* } } } */
