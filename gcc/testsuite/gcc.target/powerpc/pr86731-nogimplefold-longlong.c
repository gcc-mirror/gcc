/* PR86731.  Verify that the rs6000 gimple-folding code handles the
   left shift operation properly.  This is a testcase variation that
   explicitly disables gimple folding.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-maltivec -O3 -fwrapv -mno-fold-gimple -mpower8-vector " } */
/* { dg-prune-output "gimple folding of rs6000 builtins has been disabled." } */


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
   Noted variations:  if gimple folding is disabled, or if -fwrapv is not specified, the
   long long tests will generate a vspltisw+vsld pair, versus generating a lvx.  */
/* { dg-final { scan-assembler-times {\mvspltis[bhw]\M|\mxxspltib\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsl[bhwd]\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlvx\M} 0 } } */

