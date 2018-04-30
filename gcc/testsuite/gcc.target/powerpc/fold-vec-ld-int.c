/* Verify that overloaded built-ins for vec_ld* with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed int
testld_vsi_vsi (long long ll1, vector signed int vsi2)
{
  return vec_ld (ll1, &vsi2);
}

vector signed int
testld_vsi_si (long long ll1, signed int si)
{
  return vec_ld (ll1, &si);
}

vector unsigned int
testld_vui_vui (long long ll1, vector unsigned int vui2)
{
  return vec_ld (ll1, &vui2);
}

vector unsigned int
testld_vui_ui (long long ll1, unsigned int ui)
{
  return vec_ld (ll1, &ui);
}

vector bool int
testld_vbi_vbi (long long ll1, vector bool int vbi2)
{
  return vec_ld (ll1, &vbi2);
}

vector signed int
testld_cst_vsi (vector signed int vsi2)
{
  return vec_ld (16, &vsi2);
}

vector signed int
testld_cst_si (signed int si)
{
  return vec_ld (32, &si);
}

vector unsigned int
testld_cst_vui (vector unsigned int vui2)
{
  return vec_ld (48, &vui2);
}

vector unsigned int
testld_cst_ui (unsigned int ui)
{
  return vec_ld (64, &ui);
}

vector bool int
testld_cst_vbi (vector bool int vbi2)
{
  return vec_ld (80, &vbi2);
}

/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M|\mlxvw4x\M|\mlxv\M} 10 } } */

