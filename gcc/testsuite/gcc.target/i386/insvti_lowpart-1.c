/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(__int128 x, unsigned long long y)
{
  __int128 m = ~((__int128)~0ull);
  __int128 t = x & m;
  __int128 r = t | y;
  return r;
}

/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "orq" } } */
