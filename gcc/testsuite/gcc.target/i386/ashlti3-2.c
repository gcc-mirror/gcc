/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(__int128 x, long long y)
{
  __int128 t = (__int128)y << 64;
  return x ^ t;
}

__int128 bar(__int128 x, long long y)
{
  __int128 t = (__int128)y << 67;
  return x ^ t;
}

/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-times "xorq" 2 } } */
