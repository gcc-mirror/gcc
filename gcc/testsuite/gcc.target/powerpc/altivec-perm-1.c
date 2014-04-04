/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O -maltivec -mno-vsx" } */

typedef unsigned char V __attribute__((vector_size(16)));

V b1(V x)
{
  return __builtin_shuffle(x, (V){ 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, });
}

V b2(V x)
{
  return __builtin_shuffle(x, (V){ 2,3,2,3, 2,3,2,3, 2,3,2,3, 2,3,2,3, });
}

V b4(V x)
{
  return __builtin_shuffle(x, (V){ 4,5,6,7, 4,5,6,7, 4,5,6,7, 4,5,6,7, });
}

V h1(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 0, 16,  1, 17,  2, 18,  3, 19,  4, 20,  5, 21,  6, 22,  7, 23 });
}

V h2(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 0,  1, 16, 17,  2,  3, 18, 19,  4,  5, 20, 21,  6,  7, 22, 23 });
}

V h4(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 0,  1,  2,  3, 16, 17, 18, 19,  4,  5,  6,  7, 20, 21, 22, 23 });
}

V l1(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 8, 24,  9, 25, 10, 26, 11, 27, 12, 28, 13, 29, 14, 30, 15, 31 });
}

V l2(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 8,  9, 24, 25, 10, 11, 26, 27, 12, 13, 28, 29, 14, 15, 30, 31 });
}

V l4(V x, V y)
{
  return __builtin_shuffle(x, y,
	(V){ 8,  9, 10, 11, 24, 25, 26, 27, 12, 13, 14, 15, 28, 29, 30, 31 });
}

/* { dg-final { scan-assembler-not "vperm" } } */
/* { dg-final { scan-assembler "vspltb" } } */
/* { dg-final { scan-assembler "vsplth" } } */
/* { dg-final { scan-assembler "vspltw" } } */
