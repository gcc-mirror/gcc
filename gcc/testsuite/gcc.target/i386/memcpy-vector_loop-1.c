/* { dg-do compile } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -march=atom -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movdqa" 8 { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler-times "movdqa" 4 { target { ia32 } } } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}

