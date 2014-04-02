/* { dg-do compile } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -march=atom -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movdqa" 4} } */

char *a;
void t (void)
{
  __builtin_memset (a, 0, 2048);
}

