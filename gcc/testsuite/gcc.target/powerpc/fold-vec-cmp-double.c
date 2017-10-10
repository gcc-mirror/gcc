/* Verify that overloaded built-ins for vec_cmp with
   double inputs for VSX produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector bool long long
test2_eq (vector double x, vector double y)
{
  return vec_cmpeq (x, y);
}

vector bool long long
test2_ge (vector double x, vector double y)
{
  return vec_cmpge (x, y);
}

vector bool long long
test2_gt (vector double x, vector double y)
{
  return vec_cmpgt (x, y);
}

vector bool long long
test2_le (vector double x, vector double y)
{
  return vec_cmple (x, y);
}

vector bool long long
test2_lt (vector double x, vector double y)
{
  return vec_cmplt (x, y);
}

	vector bool long long
test2_ne (vector double x, vector double y)
{
  return vec_cmpne (x, y);
}

/* { dg-final { scan-assembler-times "xvcmpeqdp" 2 } } */
/* { dg-final { scan-assembler-times "xvcmpgtdp" 2 } } */
/* { dg-final { scan-assembler-times "xvcmpnedp" 0 } } */
/* { dg-final { scan-assembler-times "xvcmpgedp" 2 } } */
/* { dg-final { scan-assembler-times "fcmpu" 0 } } */

