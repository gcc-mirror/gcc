/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-skip-if "" { powerpc*le-*-* } { "*" } { "" } } */
/* { dg-options "-O -maltivec -mno-vsx" } */

typedef unsigned char V __attribute__((vector_size(16)));

V p2(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 1,  3,  5,  7,  9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 });

}

V p4(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 2,  3,  6,  7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27, 30, 31 });
}

/* { dg-final { scan-assembler-not "vperm" } } */
/* { dg-final { scan-assembler "vpkuhum" } } */
/* { dg-final { scan-assembler "vpkuwum" } } */
