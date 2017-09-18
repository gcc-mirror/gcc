/* Verify that overloaded built-ins for vec_pack with long long
   inputs produce the right results.  */

/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mvsx -mpower8-vector -O2" } */

#include <altivec.h>

vector bool int
testbl_h (vector bool long long vbl2, vector bool long long vbl3)
{
  return vec_pack (vbl2, vbl3);
}

vector signed int
testsl_h (vector signed long long vsl2, vector signed long long vsl3)
{
  return vec_pack (vsl2, vsl3);
}

vector unsigned int
testul_h (vector unsigned long vul2, vector unsigned long vul3)
{
  return vec_pack (vul2, vul3);
}

/* { dg-final { scan-assembler-times "vpkudum" 3 } } */
