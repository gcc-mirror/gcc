/* PR86731.  Verify that the rs6000 gimple-folding code handles the
   left shift properly.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-maltivec -O3 -mvsx " } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

vector unsigned long long splatu4(void)
{
        vector unsigned long long mzero = {-1,-1};
        return (vector unsigned long long) vec_sl(mzero, mzero);
}
vector signed long long splats4(void)
{
        vector unsigned long long mzero = {-1,-1};
        return (vector signed long long) vec_sl(mzero, mzero);
}

/* Codegen will consist of splat and shift instructions for most types.
   Noted variations:  if gimple folding is disabled, or if -fwrapv is not
   specified, the long long tests will generate a vspltisw+vsld pair,
   versus generating a single lvx.  */
/* { dg-final { scan-assembler-times {\mvspltis[bhw]\M|\mxxspltib\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsl[bhwd]\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlvx\M} 0 } } */

