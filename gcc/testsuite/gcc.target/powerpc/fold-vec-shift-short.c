/* Verify that overloaded built-ins for vec_sl with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed short
testsl_signed (vector signed short x, vector unsigned short y)
{
  return vec_sl (x, y);
}

vector unsigned short
testsl_unsigned (vector unsigned short x, vector unsigned short y)
{
  return vec_sl (x, y);
}

vector signed short
testsr_signed (vector signed short x, vector unsigned short y)
{
  return vec_sr (x, y);
}

vector unsigned short
testsr_unsigned (vector unsigned short x, vector unsigned short y)
{
  return vec_sr (x, y);
}

vector signed short
testsra_signed (vector signed short x, vector unsigned short y)
{
  return vec_sra (x, y);
}

vector unsigned short
testsra_unsigned (vector unsigned short x, vector unsigned short y)
{
  return vec_sra (x, y);
}

vector signed short
testrl_signed (vector signed short x, vector unsigned short y)
{
  return vec_rl (x, y);
}

vector unsigned short
testrl_unsigned (vector unsigned short x, vector unsigned short y)
{
  return vec_rl (x, y);
}

/* { dg-final { scan-assembler-times "vslh" 2 } } */
/* { dg-final { scan-assembler-times "vsrh" 2 } } */
/* { dg-final { scan-assembler-times "vsrah" 2 } } */
/* { dg-final { scan-assembler-times "vrlh" 2 } } */
