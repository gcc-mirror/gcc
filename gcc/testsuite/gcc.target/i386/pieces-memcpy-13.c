/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mtune=generic" } */

extern char *dst, *src;

void
foo (void)
{
  __builtin_memcpy (dst, src, 66);
}

/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*%zmm" 2 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
