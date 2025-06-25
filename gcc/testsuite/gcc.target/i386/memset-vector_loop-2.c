/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -mtune-ctrl=^sse_typeless_stores -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movdqa" 4} } */

char *a;
void t (void)
{
  __builtin_memset (a, 0, 2048);
}

