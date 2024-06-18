/* PR86731.  Verify that the rs6000 gimple-folding code handles the
   left shift properly.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O3" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-require-effective-target lp64 } */

#include <altivec.h>
/* The original test as reported.  */
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

/* Codegen will consist of splat and shift instructions for most types.
   Noted variations:  if gimple folding is disabled, or if -fwrapv is not
   specified, the long long tests will generate a vspltisw+vsld pair,
   versus generating a single lvx.  */
/* { dg-final { scan-assembler-times {\mvspltis[bhw]\M|\mxxspltib\M} 7 } } */
/* { dg-final { scan-assembler-times {\mvsl[bhwd]\M} 7 } } */
/* { dg-final { scan-assembler-times {\mlvx\M} 0 } } */

