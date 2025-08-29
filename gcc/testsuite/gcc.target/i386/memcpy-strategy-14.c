/* { dg-do compile } */
/* { dg-options "-Os -mno-avx -msse2 -mtune=generic -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movaps" 8 } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}
