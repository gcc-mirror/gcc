/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <altivec.h>

/* Test whether XXSPLTIW is generated for V4SI vector constants.  We make sure
   the power9 support (XXSPLTIB/VEXTSB2W) is not done.  */

vector int
v4si_const_1 (void)
{
  return (vector int) { 1, 1, 1, 1 };			/* VSLTPISW.  */
}

vector int
v4si_const_126 (void)
{
  return (vector int) { 126, 126, 126, 126 };		/* XXSPLTIW.  */
}

vector int
v4si_const_1023 (void)
{
  return (vector int) { 1023, 1023, 1023, 1023 };	/* XXSPLTIW.  */
}

vector int
v4si_splats_1 (void)
{
  return vec_splats (1);				/* VSLTPISW.  */
}

vector int
v4si_splats_126 (void)
{
  return vec_splats (126);				/* XXSPLTIW.  */
}

vector int
v8hi_splats_1023 (void)
{
  return vec_splats (1023);				/* XXSPLTIW.  */
}

/* { dg-final { scan-assembler-times {\mxxspltiw\M}  4 } } */
/* { dg-final { scan-assembler-times {\mvspltisw\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mxxspltib\M}    } } */
/* { dg-final { scan-assembler-not   {\mvextsb2w\M}    } } */
/* { dg-final { scan-assembler-not   {\mlxvx?\M}       } } */
/* { dg-final { scan-assembler-not   {\mplxv\M}        } } */
