/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, 3, 16);
}

/* { dg-final { scan-assembler-times "movd\[ \\t\]+\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "pshufd" 1 } } */
/* { dg-final { scan-assembler-times "movups\[\\t \]%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
