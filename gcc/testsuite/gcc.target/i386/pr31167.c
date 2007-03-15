/* { dg-do compile { target x86_64-*-* } } */
/* { dg-options "-O" } */

typedef int int32_t;

int32_t round32hi (const __int128_t arg)
{
  const int SHIFT = 96;
  const int mshift = 96;
  const __int128_t M = (~(__int128_t) 0) << mshift;
  const __int128_t L = (~M) + 1;
  const __int128_t L1 = ((__int128_t) L) >> 1;
  const __int128_t Mlo = ((__int128_t) (~M)) >> 1;
  __int128_t vv = arg & M;

  if ((arg & (L1)) && ((arg & Mlo) || (arg & L)))
    vv += L;

  return (int32_t) (vv >> SHIFT);
}
