/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <math.h>

/* Test generating SFmode constants with the ISA 3.1 (power10) XXSPLTIDP
   instruction.  */

float
scalar_float_0 (void)
{
  return 0.0f;			/* XXSPLTIB or XXLXOR.  */
}

float
scalar_float_1 (void)
{
  return 1.0f;			/* XXSPLTIDP.  */
}

#ifndef __FAST_MATH__
float
scalar_float_m0 (void)
{
  return -0.0f;			/* XXSPLTIDP.  */
}

float
scalar_float_nan (void)
{
  return __builtin_nanf ("");	/* XXSPLTIDP.  */
}

float
scalar_float_inf (void)
{
  return __builtin_inff ();	/* XXSPLTIDP.  */
}

float
scalar_float_m_inf (void)	/* XXSPLTIDP.  */
{
  return - __builtin_inff ();
}
#endif

float
scalar_float_pi (void)
{
  return (float)M_PI;		/* XXSPLTIDP.  */
}

float
scalar_float_denorm (void)
{
  return 0x1p-149f;		/* PLFS.  */
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 6 } } */
