/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(__int128 x, unsigned long long y)
{
  __int128 t = (__int128)y << 64;
  __int128 r = (x & ~0ull) | t;
  return r;
}

/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "orq" } } */
