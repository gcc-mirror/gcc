/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-final { scan-assembler "unpcklps" } } */
/* { dg-final { scan-assembler "blendps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "unpckhps" } } */

typedef float v4sf __attribute__ ((vector_size (16)));

v4sf unpcklps(v4sf a, v4sf b)
{
    return (v4sf){a[0],b[0],a[1],b[1]};
}

v4sf blendps(v4sf a, v4sf b)
{
    return (v4sf){a[0],b[1],a[2],b[3]};
}
