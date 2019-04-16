/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector double
make_doubles (__vector double *significands_p,
	      __vector unsigned long long int *exponents_p)
{
  __vector double significands = *significands_p;
  __vector unsigned long long int exponents = *exponents_p;

  return vec_insert_exp (significands, exponents);
}

int
main ()
{
  __vector unsigned long long int significands;
  __vector double *significands_p = (__vector double *) &significands;
  __vector unsigned long long int exponents;
  __vector double result;

  /* 53 bits in significand, plus the sign bit: 0x8000_0000_0000_0000 */
  significands[0] = 0x0010000000000000;	/*  1.0 */
  significands[1] = 0x801c000000000000;	/* -1.75 */

  exponents[0] = 1023;		/*  exp = 0: 1.0 */
  exponents[1] = 1021;		/* exp = -2: -0.4375 (7/16) */

  result = make_doubles (significands_p, &exponents);
  if ((result[0] != 1.0) || (result[1] != -0.4375))
    abort();
  return 0;
}

