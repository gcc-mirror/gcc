/* { dg-do compile } */
/* { dg-options "-Os -mno-avx -msse2 -mtune=generic -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movaps" 4 } } */

char a[2048];
void t (void)
{
  __builtin_memset (a, 0, 2048);
}
