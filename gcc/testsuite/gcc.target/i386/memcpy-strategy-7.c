/* { dg-do compile } */
/* { dg-options "-O2 -march=tigerlake -mno-sse" } */
/* { dg-final { scan-assembler "rep movsb" } } */

void
foo (char *dest, char *src)
{
  __builtin_memcpy (dest, src, 256);
}
