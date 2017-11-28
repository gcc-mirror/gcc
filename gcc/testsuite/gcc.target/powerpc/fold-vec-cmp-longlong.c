/* Verify that overloaded built-ins for vec_cmp with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector bool long long
test3_eq (vector signed long long x, vector signed long long y)
{
  return vec_cmpeq (x, y);
}

vector bool long long
test6_eq (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmpeq (x, y);
}

vector bool long long
test3_ge (vector signed long long x, vector signed long long y)
{
  return vec_cmpge (x, y);
}

vector bool long long
test6_ge (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmpge (x, y);
}

vector bool long long
test3_gt (vector signed long long x, vector signed long long y)
{
  return vec_cmpgt (x, y);
}

vector bool long long
test6_gt (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmpgt (x, y);
}

vector bool long long
test3_le (vector signed long long x, vector signed long long y)
{
  return vec_cmple (x, y);
}

vector bool long long
test6_le (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmple (x, y);
}

vector bool long long
test3_lt (vector signed long long x, vector signed long long y)
{
  return vec_cmplt (x, y);
}

vector bool long long
test6_lt (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmplt (x, y);
}

vector bool long long
test3_ne (vector signed long long x, vector signed long long y)
{
  return vec_cmpne (x, y);
}

vector bool long long
test6_ne (vector unsigned long long x, vector unsigned long long y)
{
  return vec_cmpne (x, y);
}

/* { dg-final { scan-assembler-times "vcmpequd" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtsd" 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtud" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 } } */

