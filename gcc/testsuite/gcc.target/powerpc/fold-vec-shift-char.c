/* Verify that overloaded built-ins for vec_sl with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

//# vec_sl  - shift left
//# vec_sr  - shift right
//# vec_sra - shift right algebraic
//# vec_rl  - rotate left

vector signed char
testsl_signed (vector signed char x, vector unsigned char y)
{
  return vec_sl (x, y);
}

vector unsigned char
testsl_unsigned (vector unsigned char x, vector unsigned char y)
{
  return vec_sl (x, y);
}

vector signed char
testsr_signed (vector signed char x, vector unsigned char y)
{
  return vec_sr (x, y);
}

vector unsigned char
testsr_unsigned (vector unsigned char x, vector unsigned char y)
{
  return vec_sr (x, y);
}

vector signed char
testsra_signed (vector signed char x, vector unsigned char y)
{
  return vec_sra (x, y);
}

vector unsigned char
testsra_unsigned (vector unsigned char x, vector unsigned char y)
{
  return vec_sra (x, y);
}

vector signed char
testrl_signed (vector signed char x, vector unsigned char y)
{
  return vec_rl (x, y);
}

vector unsigned char
testrl_unsigned (vector unsigned char x, vector unsigned char y)
{
  return vec_rl (x, y);
}

/* { dg-final { scan-assembler-times "vslb" 2 } } */
/* { dg-final { scan-assembler-times "vsrb" 2 } } */
/* { dg-final { scan-assembler-times "vsrab" 2 } } */
/* { dg-final { scan-assembler-times "vrlb" 2 } } */
