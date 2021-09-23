/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512f -mtune=generic" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 0, 66);
}

/* { dg-final { scan-assembler-times "vpxor(?:d|)\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu(?:64|8)\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* { dg-final { scan-assembler-not "vzeroupper" } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
