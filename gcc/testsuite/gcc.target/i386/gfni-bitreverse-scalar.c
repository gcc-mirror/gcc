/* { dg-do compile } */
/* { dg-options "-mgfni -mno-avx512bmm -O2" } */
/* { dg-final { scan-assembler-times "gf2p8affineqb" 4 } } */
/* { dg-final { scan-assembler-not "vbitrevb" } } */
/* { dg-final { scan-assembler-not "vgf2p8affineqb" } } */

/* GFNI-only scalar bitreverse must retain the existing legacy GFNI path.  */
unsigned char
br8 (unsigned char x)
{
  return __builtin_bitreverse8 (x);
}

unsigned short
br16 (unsigned short x)
{
  return __builtin_bitreverse16 (x);
}

unsigned int
br32 (unsigned int x)
{
  return __builtin_bitreverse32 (x);
}

unsigned long long
br64 (unsigned long long x)
{
  return __builtin_bitreverse64 (x);
}
