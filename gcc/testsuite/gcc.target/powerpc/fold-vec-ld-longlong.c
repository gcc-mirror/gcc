/* Verify that overloaded built-ins for vec_ld* with long long
   inputs produce the right code.  */

/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector signed long long
testld_vsl_vsl (long long ll1, vector signed long vsl2)
{
  return vec_ld (ll1, &vsl2);
}

vector unsigned long long
testld_vul_vul (long long ll1, vector unsigned long vul2)
{
  return vec_ld (ll1, &vul2);
}

vector bool long long
testld_vbl_vbl (long long ll1, vector bool long vbl2)
{
  return vec_ld (ll1, &vbl2);
}

vector signed long long
testld_cst_vsl (vector signed long vsl2)
{
  return vec_ld (16, &vsl2);
}

vector unsigned long long
testld_cst_vul (vector unsigned long vul2)
{
  return vec_ld (32, &vul2);
}

vector bool long long
testld_cst_vbl (vector bool long vbl2)
{
  return vec_ld (48, &vbl2);
}

/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M} 6 } } */

