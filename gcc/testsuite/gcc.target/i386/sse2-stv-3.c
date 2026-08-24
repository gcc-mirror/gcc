/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mno-sse4 -mno-stackrealign" } */

__int128 a, b, c, z;

__int128 fun();

void foo_in(__int128 x)
{
  z = (x ^ a ^ b ^ c);
}

__int128 foo_out()
{
  return (z ^ a ^ b ^ c);
}

__int128 foo_inout(__int128 x)
{
  return (x ^ a ^ b ^ c ^ z);
}

void foo_fun()
{
  z = (fun() ^ a ^ b ^ c);
}

/* { dg-final { scan-assembler-times "punpcklqdq" 3 } } */
/* { dg-final { scan-assembler-times "movhlps" 2 } } */
/* { dg-final { scan-assembler-times "pxor" 13 } } */
/* { dg-final { scan-assembler-not "xorq" } } */
