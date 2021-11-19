/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#include <altivec.h>
void
bug (__vector_pair *dst)
{
  register vector unsigned char vec0 asm ("vs44");
  register vector unsigned char vec1 asm ("vs32");
  __builtin_vsx_build_pair (dst, vec0, vec1);
}

/* { dg-final { scan-assembler-times {(?p)\mxxlor \d+,44,44\M} 1 } } */
/* { dg-final { scan-assembler-times {(?p)\mxxlor \d+,32,32\M} 1 } } */
