/* cross section of shift tests specific for shift-left.
 * This is a counterpart to the fold-vec-shift-left-frwapv test.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed char
testsl_signed_char (vector signed char x, vector unsigned char y)
{
  return vec_sl (x, y);
}

vector unsigned char
testsl_unsigned_char (vector unsigned char x, vector unsigned char y)
{
  return vec_sl (x, y);
}

vector signed short
testsl_signed_short (vector signed short x, vector unsigned short y)
{
  return vec_sl (x, y);
}

vector unsigned short
testsl_unsigned_short (vector unsigned short x, vector unsigned short y)
{
  return vec_sl (x, y);
}

vector signed int
testsl_signed_int (vector signed int x, vector unsigned int y)
{
  return vec_sl (x, y);
}

vector unsigned int
testsl_unsigned_int (vector unsigned int x, vector unsigned int y)
{
  return vec_sl (x, y);
}

/* { dg-final { scan-assembler-times "vslb" 2 } } */
/* { dg-final { scan-assembler-times "vslh" 2 } } */
/* { dg-final { scan-assembler-times "vslw" 2 } } */
