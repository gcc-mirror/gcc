/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <altivec.h>

/* Test whether XXSPLTIW is generated for V8HI vector constants.  We make sure
   the power9 support (XXSPLTIB/VUPKLSB) is not done.  */

vector short
v8hi_const_1 (void)
{
  return (vector short) { 1, 1, 1, 1, 1, 1, 1, 1 };	/* VSLTPISH.  */
}

vector short
v8hi_const_126 (void)
{
  return (vector short) { 126, 126, 126, 126,
			  126, 126, 126, 126 };		/* XXSPLTIW.  */
}

vector short
v8hi_const_1023 (void)
{
  return (vector short) { 1023, 1023, 1023, 1023,
			  1023, 1023, 1023, 1023 };	/* XXSPLTIW.  */
}

vector short
v8hi_splats_1 (void)
{
  return vec_splats ((short)1);				/* VSLTPISH.  */
}

vector short
v8hi_splats_126 (void)
{
  return vec_splats ((short)126);			/* XXSPLTIW.  */
}

vector short
v8hi_splats_1023 (void)
{
  return vec_splats ((short)1023);			/* XXSPLTIW.  */
}

/* Test that we can optimiza V8HI where all of the even elements are the same
   and all of the odd elements are the same.  */
vector short
v8hi_const_1023_1000 (void)
{
  return (vector short) { 1023, 1000, 1023, 1000,
			  1023, 1000, 1023, 1000 };	/* XXSPLTIW.  */
}

/* { dg-final { scan-assembler-times {\mxxspltiw\M}  5 } } */
/* { dg-final { scan-assembler-times {\mvspltish\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mxxspltib\M}    } } */
/* { dg-final { scan-assembler-not   {\mvupklsb\M}     } } */
/* { dg-final { scan-assembler-not   {\mlxvx?\M}       } } */
/* { dg-final { scan-assembler-not   {\mplxv\M}        } } */
