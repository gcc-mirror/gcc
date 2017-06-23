/* Verify that overloaded built-ins for vec_sl with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector signed long long
testsl_signed (vector signed long long x, vector unsigned long long y)
{
  return vec_sl (x, y);
}

vector unsigned long long
testsl_unsigned (vector unsigned long long x, vector unsigned long long y)
{
  return vec_sl (x, y);
}

vector signed long long
testsr_signed (vector signed long long x, vector unsigned long long y)
{
  return vec_sr (x, y);
}

vector unsigned long long
testsr_unsigned (vector unsigned long long x, vector unsigned long long y)
{
  return vec_sr (x, y);
}

vector signed long long
testsra_signed (vector signed long long x, vector unsigned long long y)
{
  return vec_sra (x, y);
}

/* watch for PR 79544 here (vsrd / vsrad issue) */
vector unsigned long long
testsra_unsigned (vector unsigned long long x, vector unsigned long long y)
{
  return vec_sra (x, y);
}

vector signed long long
testrl_signed (vector signed long long x, vector unsigned long long y)
{
  return vec_rl (x, y);
}

vector unsigned long long
testrl_unsigned (vector unsigned long long x, vector unsigned long long y)
{
  return vec_rl (x, y);
}

/* { dg-final { scan-assembler-times "vsld" 2 } } */
/* { dg-final { scan-assembler-times "vsrd" 2 } } */
/* { dg-final { scan-assembler-times "vsrad" 2 } } */
/* { dg-final { scan-assembler-times "vrld" 2 } } */

