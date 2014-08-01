#ifdef __UINT64_TYPE__
typedef __UINT64_TYPE__ uint64_t;
#else
typedef unsigned long long uint64_t;
#endif

#ifndef __SIZEOF_INT128__
#define __int128 long long
#endif

/* Some version of bswap optimization would ICE when analyzing a mask constant
   too big for an HOST_WIDE_INT (PR61375).  */

__attribute__ ((noinline, noclone)) uint64_t
uint128_central_bitsi_ior (unsigned __int128 in1, uint64_t in2)
{
  __int128 mask = (__int128)0xffff << 56;
  return ((in1 & mask) >> 56) | in2;
}

int
main (int argc)
{
  __int128 in = 1;
#ifdef __SIZEOF_INT128__
  in <<= 64;
#endif
  if (sizeof (uint64_t) * __CHAR_BIT__ != 64)
    return 0;
  if (sizeof (unsigned __int128) * __CHAR_BIT__ != 128)
    return 0;
  if (uint128_central_bitsi_ior (in, 2) != 0x102)
    __builtin_abort ();
  return 0;
}
