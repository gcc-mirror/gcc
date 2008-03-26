/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

struct SubData
{
    inline const Float Clamp(Float f, Float f0, Float f1) // { dg-error "" }
    }
    inline const void SinCos(Float angle, Float& sine, Float& cosine) // { dg-error "" }
    {
        C0 = __builtin_vec_splat(_simdCosEstCoefficients, 0); // { dg-error "" }
        C1 = __builtin_vec_splat(_simdCosEstCoefficients, 1); // { dg-error "" }

