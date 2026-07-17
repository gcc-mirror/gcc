/* Header file for pr102976.c test - contains test functions only */

#include <altivec.h>
void
bug (__vector_pair *dst)
{
  register vector unsigned char vec0 asm ("vs44");
  register vector unsigned char vec1 asm ("vs32");
  __builtin_vsx_build_pair (dst, vec0, vec1);
}
