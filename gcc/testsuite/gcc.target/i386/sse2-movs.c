/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */
/* { dg-final { scan-assembler "movss" } } */
/* { dg-final { scan-assembler "movsd" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "shufpd" } } */

typedef float v4sf __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

v4sf movss(v4sf a, v4sf b)
{
     return (v4sf){b[0],a[1],a[2],a[3]};
}

v2df movsd(v2df a, v2df b)
{
     return (v2df){b[0],a[1]};
}
