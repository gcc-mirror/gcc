/* { dg-do compile } */
/* { dg-options "-O2 -march=tigerlake -mno-sse" } */
/* { dg-final { scan-assembler "rep stosb" } } */

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 256);
}
