/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <math.h>

/* Test generating V2DFmode constants with the ISA 3.1 (power10) XXSPLTIDP
   instruction.  */

vector double
v2df_double_0 (void)
{
  return (vector double) { 0.0, 0.0 };			/* XXSPLTIB or XXLXOR.  */
}

vector double
v2df_double_1 (void)
{
  return (vector double) { 1.0, 1.0 };			/* XXSPLTIDP.  */
}

#ifndef __FAST_MATH__
vector double
v2df_double_m0 (void)
{
  return (vector double) { -0.0, -0.0 };		/* XXSPLTIDP.  */
}

vector double
v2df_double_nan (void)
{
  return (vector double) { __builtin_nan (""),
			   __builtin_nan ("") };	/* XXSPLTIDP.  */
}

vector double
v2df_double_inf (void)
{
  return (vector double) { __builtin_inf (),
			   __builtin_inf () };		/* XXSPLTIDP.  */
}

vector double
v2df_double_m_inf (void)
{
  return (vector double) { - __builtin_inf (),
			   - __builtin_inf () };	/* XXSPLTIDP.  */
}
#endif

vector double
v2df_double_pi (void)
{
  return (vector double) { M_PI, M_PI };		/* PLVX.  */
}

vector double
v2df_double_denorm (void)
{
  return (vector double) { (double)0x1p-149f,
			   (double)0x1p-149f };		/* PLVX.  */
}

/* { dg-final { scan-assembler-times {\mxxspltidp\M} 5 } } */
