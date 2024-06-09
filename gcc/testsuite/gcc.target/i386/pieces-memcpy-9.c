/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mtune=generic" } */
/* Cope with --enable-frame-pointer, Solaris/x86 -mstackrealign default.  */
/* { dg-additional-options "-fomit-frame-pointer -mno-stackrealign" } */

void
foo (int a1, int a2, int a3, int a4, int a5, int a6, char *dst, char *src)
{
  __builtin_memcpy (dst, src, 19);
}

/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%xmm" 2 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
