/* Verify that overloaded built-ins for vec_ld* with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed char
testld_sc_vsc (long long ll1, vector signed char vsc2)
{
  return vec_ld (ll1, &vsc2);
}

vector signed char
testld_sc_sc (long long ll1, signed char sc)
{
  return vec_ld (ll1, &sc);
}

vector unsigned char
testld_uc_vuc (long long ll1, vector unsigned char vuc2)
{
  return vec_ld (ll1, &vuc2);
}

vector unsigned char
testld_uc_uc (long long ll1, unsigned char uc)
{
  return vec_ld (ll1, &uc);
}

vector bool char
testld_bc_vbc (long long ll1, vector bool char vbc2)
{
  return vec_ld (ll1, &vbc2);
}

vector signed char
testld_cst_vsc (vector signed char vsc2)
{
  return vec_ld (16, &vsc2);
}

vector signed char
testld_cst_sc (signed char sc)
{
  return vec_ld (32, &sc);
}

vector unsigned char
testld_cst_vuc (vector unsigned char vuc2)
{
  return vec_ld (48, &vuc2);
}

vector unsigned char
testld_cst_uc (unsigned char uc)
{
  return vec_ld (64, &uc);
}

vector bool char
testld_cst_vbc (vector bool char vbc2)
{
  return vec_ld (80, &vbc2);
}

/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M|\mlxvw4x\M|\mlxv\M} 10 } } */

