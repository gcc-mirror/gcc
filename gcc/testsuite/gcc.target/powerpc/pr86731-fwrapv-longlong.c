/* PR86731.  Verify that the rs6000 gimple-folding code handles the
   left shift operation properly.  This is a testcase variation that
   explicitly specifies -fwrapv, which is a condition for the
   gimple folding of the vec_sl() intrinsic.  */

/* specify -mpower8-vector, which provides vec_sl(long long,...) support. */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-maltivec -O3 -fwrapv -mpower8-vector " } */

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
   If folding is enabled, the vec_sl tests using vector long long type will
   generate a lvx instead of a vspltisw+vsld pair.  */

/* { dg-final { scan-assembler-times {\mvspltis[bhw]\M} 0 } } */
/* { dg-final { scan-assembler-times {\mvsl[bhwd]\M} 0 } } */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxv\M|\mlxvd2x\M} 2 } } */

