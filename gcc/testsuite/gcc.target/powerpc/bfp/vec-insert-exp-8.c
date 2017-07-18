/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector float
make_floats (__vector unsigned int *significands_p,
	     __vector unsigned int *exponents_p)
{
  __vector unsigned int significands = *significands_p;
  __vector unsigned int exponents = *exponents_p;

  return vec_insert_exp (significands, exponents);
}

int
main ()
{
  __vector unsigned int significands;
  __vector unsigned int exponents;
  __vector float result;

  /* 24 bits in significand, plus the sign bit: 0x80ffffff */
  significands[0] = 0x00800000;	/* 1.0 */
  significands[1] = 0x00c00000;	/* 1.5 */
  significands[2] = 0x80e00000;	/* -1.75 */
  significands[3] = 0x80c00000;	/* -1.5 */

  exponents[0] = 127;		/*  exp = 0: 1.0 */
  exponents[1] = 128;		/*  exp = 1: 3.0.0 */
  exponents[2] = 129;		/*  exp = 2: -7.0 */
  exponents[3] = 125;		/* exp = -2: -0.375 */

  result = make_floats (&significands, &exponents);
  if ((result[0] != 1.0f) ||
      (result[1] != 3.0f) || (result[2] != -7.0f) || (result[3] != -0.375f))
    abort();
  return 0;
}

