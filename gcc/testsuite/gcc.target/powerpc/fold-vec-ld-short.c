/* Verify that overloaded built-ins for vec_ld* with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed short
testld_vss_vss (long long ll1, vector signed short vss2)
{
  return vec_ld (ll1, &vss2);
}

vector signed short
testld_vss_ss (long long ll1, signed short ss)
{
  return vec_ld (ll1, &ss);
}

vector unsigned short
testld_vus_vus (long long ll1, vector unsigned short vus2)
{
  return vec_ld (ll1, &vus2);
}

vector unsigned short
testld_vus_us (long long ll1, unsigned short us)
{
  return vec_ld (ll1, &us);
}

vector bool short
testld_vbs_vbs (long long ll1, vector bool short vbs2)
{
  return vec_ld (ll1, &vbs2);
}

vector signed short
testld_cst_vss (vector signed short vss2)
{
  return vec_ld (16, &vss2);
}

vector signed short
testld_cst_ss (signed short ss)
{
  return vec_ld (32, &ss);
}

vector unsigned short
testld_cst_vus (vector unsigned short vus2)
{
  return vec_ld (48, &vus2);
}

vector unsigned short
testld_cst_us (unsigned short us)
{
  return vec_ld (64, &us);
}

vector bool short
testld_cst_vbs (vector bool short vbs2)
{
  return vec_ld (80, &vbs2);
}

/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M|\mlxvw4x\M} 10 } } */

