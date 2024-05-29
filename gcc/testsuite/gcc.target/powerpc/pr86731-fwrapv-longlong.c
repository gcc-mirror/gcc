/* PR86731.  Verify that the rs6000 gimple-folding code handles the
   left shift operation properly.  This is a testcase variation that
   explicitly specifies -fwrapv, which is a condition for the
   gimple folding of the vec_sl() intrinsic.  */

/* specify -mcpu=power8 -mvsx, which provides vec_sl(long long,...) support. */

/* { dg-do compile } */
/* { dg-options "-maltivec -O3 -fwrapv -mvsx " } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target lp64 } */

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

/* Codegen will consist of splat and shift instructions for most types.  If
   folding is enabled, the vec_sl tests using vector long long type will
   generate a lvx instead of a vspltisw+vsld pair.  On power10, it will
   generate a xxspltidp instruction instead of the lvx.  */

/* { dg-final { scan-assembler-times {\mvspltis[bhw]\M} 0 } } */
/* { dg-final { scan-assembler-times {\mvsl[bhwd]\M} 0 } } */
/* { dg-final { scan-assembler-times {\mp?lxv\M|\mlvx\M|\mlxvd2x\M|\mxxspltidp\M} 2 } } */

