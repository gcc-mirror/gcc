/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -march=atom -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movdqa" 4 } } */

char a[2048];
void t (void)
{
  __builtin_memset (a, 0, 2048);
}


