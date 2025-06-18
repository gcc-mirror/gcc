/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -mtune-ctrl=^sse_typeless_stores -minline-all-stringops -mstringop-strategy=vector_loop" } */
/* { dg-final { scan-assembler-times "movdqa" 4 } } */

char *a;
char *b;
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}


