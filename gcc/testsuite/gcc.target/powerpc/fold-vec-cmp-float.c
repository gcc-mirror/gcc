/* Verify that overloaded built-ins for vec_cmp with float
   inputs for VSX produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector bool int
test1_eq (vector float x, vector float y)
{
  return vec_cmpeq (x, y);
}

vector bool int
test1_ge (vector float x, vector float y)
{
  return vec_cmpge (x, y);
}

vector bool int
test1_gt (vector float x, vector float y)
{
  return vec_cmpgt (x, y);
}

vector bool int
test1_le (vector float x, vector float y)
{
  return vec_cmple (x, y);
}

vector bool int
test1_lt (vector float x, vector float y)
{
  return vec_cmplt (x, y);
}

vector bool int
test1_ne (vector float x, vector float y)
{
  return vec_cmpne (x, y);
}

/* { dg-final { scan-assembler-times "xvcmpeqsp" 2 } } */
/* { dg-final { scan-assembler-times "xvcmpgtsp" 2 } } */
/* { dg-final { scan-assembler-times "xvcmpnesp" 0 } } */
/* { dg-final { scan-assembler-times "xvcmpgesp" 2 } } */
/* { dg-final { scan-assembler-times "fcmpu" 0 } } */

