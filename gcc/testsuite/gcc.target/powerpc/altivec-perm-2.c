/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O -maltivec -mno-vsx" } */

typedef unsigned short V __attribute__((vector_size(16)));

V f2(V x)
{
  return __builtin_shuffle(x, (V){ 1,1,1,1, 1,1,1,1, });
}

V f4(V x)
{
  return __builtin_shuffle(x, (V){ 2,3,2,3, 2,3,2,3, });
}

/* { dg-final { scan-assembler-not "vperm" } } */
/* { dg-final { scan-assembler "vsplth" } } */
/* { dg-final { scan-assembler "vspltw" } } */
