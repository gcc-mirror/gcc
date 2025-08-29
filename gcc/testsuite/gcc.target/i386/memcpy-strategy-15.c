/* { dg-do compile } */
/* { dg-options "-Os -mno-avx -msse2 -mtune=generic -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movups" 8 } } */

char *a;
char *b;
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}
