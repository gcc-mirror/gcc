/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Basic test for the new VMX intrinsics.  */
#include <altivec.h>

int f(vector int a, int b)
{
  return vec_extract (a, b);
}
short f1(vector short a, int b)
{
  return vec_extract (a, b);
}
vector short f2(vector short a, int b)
{
  return vec_insert (b, a, b);
}
vector float f3(vector float a, int b)
{
  return vec_insert (b, a, b);
}

float g(void);

vector float f4(float b, int t)
{
  return vec_promote (g(), t);
}
vector float f5(float b)
{
  return vec_splats (g());
}
