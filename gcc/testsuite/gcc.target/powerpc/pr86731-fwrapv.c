/* PR86731.  Verify that the rs6000 gimple-folding code handles the
   left shift operation properly.  This is a testcase variation that
   explicitly specifies -fwrapv, which is a condition for the
   gimple folding of the vec_sl() intrinsic.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-maltivec -O3 -fwrapv " } */

#include <altivec.h>
/* original test as reported.  */
vector unsigned int splat(void)
{
        vector unsigned int mzero = vec_splat_u32(-1);
        return (vector unsigned int) vec_sl(mzero, mzero);
}

/* more testcase variations.  */
vector unsigned char splatu1(void)
{
        vector unsigned char mzero = vec_splat_u8(-1);
        return (vector unsigned char) vec_sl(mzero, mzero);
}

vector unsigned short splatu2(void)
{
        vector unsigned short mzero = vec_splat_u16(-1);
        return (vector unsigned short) vec_sl(mzero, mzero);
}

vector unsigned int splatu3(void)
{
        vector unsigned int mzero = vec_splat_u32(-1);
        return (vector unsigned int) vec_sl(mzero, mzero);
}

vector signed char splats1(void)
{
        vector unsigned char mzero = vec_splat_u8(-1);
        return (vector signed char) vec_sl(mzero, mzero);
}

vector signed short splats2(void)
{
        vector unsigned short mzero = vec_splat_u16(-1);
        return (vector signed short) vec_sl(mzero, mzero);
}

vector signed int splats3(void)
{
        vector unsigned int mzero = vec_splat_u32(-1);
        return (vector signed int) vec_sl(mzero, mzero);
}

/* Codegen will consist of splat and shift instructions.
   If folding is enabled, the vec_sl tests using vector long long type will
   generate a lvx instead of a vspltisw+vsld pair.  */

/* { dg-final { scan-assembler-times {\mvspltis[bhw]\M|\mxxspltib\M} 7 } } */
/* { dg-final { scan-assembler-times {\mvsl[bhwd]\M} 7 } } */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M} 0 } } */

