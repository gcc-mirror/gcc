/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx2 -mavx -mtune=sandybridge" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 33);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%xmm" 4 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
