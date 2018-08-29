/* Verify that overloaded built-ins for vec_sl with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed int
testsl_signed (vector signed int x, vector unsigned int y)
{
  return vec_sl (x, y);
}

vector unsigned int
testsl_unsigned (vector unsigned int x, vector unsigned int y)
{
  return vec_sl (x, y);
}

vector signed int
testsr_signed (vector signed int x, vector unsigned int y)
{
  return vec_sr (x, y);
}

vector unsigned int
testsr_unsigned (vector unsigned int x, vector unsigned int y)
{
  return vec_sr (x, y);
}

vector signed int
testsra_signed (vector signed int x, vector unsigned int y)
{
  return vec_sra (x, y);
}

vector unsigned int
testsra_unsigned (vector unsigned int x, vector unsigned int y)
{
  return vec_sra (x, y);
}

vector signed int
testrl_signed (vector signed int x, vector unsigned int y)
{
  return vec_rl (x, y);
}

vector unsigned int
testrl_unsigned (vector unsigned int x, vector unsigned int y)
{
  return vec_rl (x, y);
}

/* { dg-final { scan-assembler-times "vslw" 2 } } */
/* { dg-final { scan-assembler-times "vsrw" 2 } } */
/* { dg-final { scan-assembler-times "vsraw" 2 } } */
/* { dg-final { scan-assembler-times "vrlw" 2 } } */
