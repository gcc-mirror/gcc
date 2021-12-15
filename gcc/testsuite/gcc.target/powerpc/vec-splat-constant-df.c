/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <math.h>

/* Test generating DFmode constants with the ISA 3.1 (power10) XXSPLTIDP
   instruction.  */

double
scalar_double_0 (void)
{
  return 0.0;			/* XXSPLTIB or XXLXOR.  */
}

double
scalar_double_1 (void)
{
  return 1.0;			/* XXSPLTIDP.  */
}

#ifndef __FAST_MATH__
double
scalar_double_m0 (void)
{
  return -0.0;			/* XXSPLTIDP.  */
}

double
scalar_double_nan (void)
{
  return __builtin_nan ("");	/* XXSPLTIDP.  */
}

double
scalar_double_inf (void)
{
  return __builtin_inf ();	/* XXSPLTIDP.  */
}

double
scalar_double_m_inf (void)	/* XXSPLTIDP.  */
{
  return - __builtin_inf ();
}
#endif

double
scalar_double_pi (void)
{
  return M_PI;			/* PLFD.  */
}

double
scalar_double_denorm (void)
{
  return 0x1p-149f;		/* PLFD.  */
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 5 } } */
