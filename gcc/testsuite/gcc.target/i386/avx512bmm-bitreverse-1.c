/* { dg-do compile } */
/* { dg-options "-mavx512bmm -mavx512vl -mno-gfni -O2" } */
/* { dg-final { scan-assembler-times "vbitrevb" 4 } } */
/* { dg-final { scan-assembler-not "gf2p8affineqb" } } */

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
