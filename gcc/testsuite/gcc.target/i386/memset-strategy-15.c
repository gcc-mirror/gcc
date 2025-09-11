/* { dg-do compile } */
/* { dg-options "-Os -mno-avx -msse2 -mtune=generic -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movups" 4} } */

char *a;
void t (void)
{
  __builtin_memset (a, 0, 2048);
}
