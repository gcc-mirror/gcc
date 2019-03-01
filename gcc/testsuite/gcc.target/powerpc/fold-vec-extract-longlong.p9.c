/* Verify that overloaded built-ins for vec_extract() with long long
   inputs produce the right code for a P9 (LE) target.  */

/* { dg-do compile { target { powerpc*-*-linux* && le } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

// targeting P9 (LE), six tests.
// p9 constants: mfvsrd
// p9 vars:  xori, rldic, mtvsrdd, vslo, mfvsrd

/* results. */
/* { dg-final { scan-assembler-times {\mxori\M} 3 } } */
/* { dg-final { scan-assembler-times {\mrldic\M} 3 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 3 } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M} 6 } } */

#include <altivec.h>

unsigned long long
testbl_var (vector bool long long vbl2, signed int si)
{
  return vec_extract (vbl2, si);
}

signed long long
testsl_var (vector signed long long vsl2, signed int si)
{
  return vec_extract (vsl2, si);
}

unsigned long long
testul_var (vector unsigned long long vul2, signed int si)
{
  return vec_extract (vul2, si);
}

unsigned long long
testbl_cst (vector bool long long vbl2)
{
  return vec_extract (vbl2, 1);
}

signed long long
testsl_cst (vector signed long long vsl2)
{
  return vec_extract (vsl2, 1);
}

unsigned long long
testul_cst (vector unsigned long vul2)
{
  return vec_extract (vul2, 1);
}

