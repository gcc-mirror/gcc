/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -march=atom -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-not "movdqa" } } */

char *a;
char *b;
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}
