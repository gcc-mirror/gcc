/* Test __builtin_bswapg.  */
/* { dg-do run { target { c || c++11 } } } */
/* { dg-options "-O2" } */

#if !__has_builtin(__builtin_bswapg)
#error __builtin_bswapg unsupported
#endif

#if __CHAR_BIT__ == 8
#if __SIZEOF_INT__ == 4
static_assert (__builtin_bswapg (0x12345678U) == 0x78563412U, "");
#endif
#if __SIZEOF_LONG_LONG__ == 8
static_assert (__builtin_bswapg (0x123456789abcdef0ULL) == 0xf0debc9a78563412ULL, "");
#endif
static_assert (__builtin_bswapg ((unsigned char) 0x23) == (unsigned char) 0x23, "");
#if __SIZEOF_SHORT__ == 2
static_assert (__builtin_bswapg ((unsigned short) 0x2345) == (unsigned short) 0x4523, "");
#endif
#if __SIZEOF_INT128__ == 16
static_assert (__builtin_bswapg ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
				 | 0x23456789abcdef01ULL)
				 == ((((unsigned __int128) 0x01efcdab89674523ULL) << 64)
				     | 0xf0debc9a78563412ULL), "");
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
static_assert (__builtin_bswapg (0xabcdefuwb) == 0xefcdabuwb, "");
static_assert (__builtin_bswapg (0xabcdef8081uwb) == 0x8180efcdabuwb, "");
static_assert (__builtin_bswapg ((unsigned _BitInt(136)) 0x66b52574b5fd190b15a1b99858c27d0ac2uwb)
	       == 0xc20a7dc25898b9a1150b19fdb57425b566uwb, "");
static_assert (__builtin_bswapg ((unsigned _BitInt(832)) 0x021256fc7a59ee4dab277edcadfdbfd45c0690f99249cf9e31ad0b16f91a0efb2d64f8a7bde6f4680d7244ba70038ecc091fc59c68943a994146b03a60be368d222e34e0142948229b8ddc0b906bd79a8a9c36f230708b71b63bac17f9e2a41c10d9fc240244469duwb)
	       == 0x9d46440224fcd9101ca4e2f917ac3bb6718b7030f2369c8a9ad76b900bdc8d9b22482914e0342e228d36be603ab04641993a94689cc51f09cc8e0370ba44720d68f4e6bda7f8642dfb0e1af9160bad319ecf4992f990065cd4bffdaddc7e27ab4dee597afc561202uwb, "");
static_assert (__builtin_bswapg ((unsigned _BitInt(840)) 0x4c23edce0b5ffd538da2112c333d17f100a0db44f7c9cfb097538e995f3a5bab5d487969a776bc3518cd614b0a783d0b18184f10e11ec078714f11d0896c7be6b5f54f5c6b0c9184d9f3cfe46b0cef84243e94055d547dcea38f9f2cfb1a13b00f620d7b6e9eff4891uwb)
	       == 0x9148ff9e6e7b0d620fb0131afb2c9f8fa3ce7d545d05943e2484ef0c6be4cff3d984910c6b5c4ff5b5e67b6c89d0114f7178c01ee1104f18180b3d780a4b61cd1835bc76a76979485dab5b3a5f998e5397b0cfc9f744dba000f1173d332c11a28d53fd5f0bceed234cuwb, "");
static_assert (__builtin_bswapg ((unsigned _BitInt(856)) 0x41c39e2f987be9c81688ecc08a4ded319e4010952ecb6e4ac738a6924d81668f4404bedd7fa7a5952f9156bade43eb7d83fb12b2c780fc8eb9a22006238695678a53440afbe99f2a7af5e45938cfd6bccbe86e92696a20220c41282fc7be965211db5c0cc4fb373ff11b98uwb)
	       == 0x981bf13f37fbc40c5cdb115296bec72f28410c22206a69926ee8cbbcd6cf3859e4f57a2a9fe9fb0a44538a679586230620a2b98efc80c7b212fb837deb43deba56912f95a5a77fddbe04448f66814d92a638c74a6ecb2e9510409e31ed4d8ac0ec8816c8e97b982f9ec341uwb, "");
#endif

#if __CHAR_BIT__ == 8
#if __SIZEOF_INT__ == 4
[[gnu::noipa]] unsigned
bswap32 (unsigned x)
{
  return __builtin_bswapg (x);
}
#endif
#if __SIZEOF_LONG_LONG__ == 8
[[gnu::noipa]] unsigned long long
bswap64 (unsigned long long x)
{
  return __builtin_bswapg (x);
}
#endif
[[gnu::noipa]] unsigned char
bswap8 (unsigned char x)
{
  return __builtin_bswapg (x);
}
#if __SIZEOF_SHORT__ == 2
[[gnu::noipa]] unsigned short
bswap16 (unsigned short x)
{
  return __builtin_bswapg (x);
}
#endif
#if __SIZEOF_INT128__ == 16
[[gnu::noipa]] unsigned __int128
bswap128 (unsigned __int128 x)
{
  return __builtin_bswapg (x);
}
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
[[gnu::noipa]] unsigned _BitInt(24)
bswap24 (unsigned _BitInt(24) x)
{
  return __builtin_bswapg (x);
}

[[gnu::noipa]] unsigned _BitInt(40)
bswap40 (unsigned _BitInt(40) x)
{
  return __builtin_bswapg (x);
}

[[gnu::noipa]] unsigned _BitInt(136)
bswap136 (unsigned _BitInt(136) x)
{
  return __builtin_bswapg (x);
}

[[gnu::noipa]] unsigned _BitInt(832)
bswap832 (unsigned _BitInt(832) x)
{
  return __builtin_bswapg (x);
}

[[gnu::noipa]] unsigned _BitInt(840)
bswap840 (unsigned _BitInt(840) x)
{
  return __builtin_bswapg (x);
}

[[gnu::noipa]] unsigned _BitInt(856)
bswap856 (unsigned _BitInt(856) x)
{
  return __builtin_bswapg (x);
}
#endif

int
main ()
{
#if __CHAR_BIT__ == 8
#if __SIZEOF_INT__ == 4
  if (bswap32 (0x12345678U) != 0x78563412U)
    __builtin_abort ();
#endif
#if __SIZEOF_LONG_LONG__ == 8
  if (bswap64 (0x123456789abcdef0ULL) != 0xf0debc9a78563412ULL)
    __builtin_abort ();
#endif
  if (bswap8 ((unsigned char) 0x23) != (unsigned char) 0x23)
    __builtin_abort ();
#if __SIZEOF_SHORT__ == 2
  if (bswap16 ((unsigned short) 0x2345) != (unsigned short) 0x4523)
    __builtin_abort ();
#endif
#if __SIZEOF_INT128__ == 16
  if (bswap128 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
		| 0x23456789abcdef01ULL)
      != ((((unsigned __int128) 0x01efcdab89674523ULL) << 64)
	  | 0xf0debc9a78563412ULL))
    __builtin_abort ();
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
  if (bswap24 (0xabcdefuwb) != 0xefcdabuwb)
    __builtin_abort ();
  if (bswap40 (0xabcdef8081uwb) != 0x8180efcdabuwb)
    __builtin_abort ();
  if (bswap136 (0x66b52574b5fd190b15a1b99858c27d0ac2uwb)
      != 0xc20a7dc25898b9a1150b19fdb57425b566uwb)
    __builtin_abort ();
  if (bswap832 (0x021256fc7a59ee4dab277edcadfdbfd45c0690f99249cf9e31ad0b16f91a0efb2d64f8a7bde6f4680d7244ba70038ecc091fc59c68943a994146b03a60be368d222e34e0142948229b8ddc0b906bd79a8a9c36f230708b71b63bac17f9e2a41c10d9fc240244469duwb)
      != 0x9d46440224fcd9101ca4e2f917ac3bb6718b7030f2369c8a9ad76b900bdc8d9b22482914e0342e228d36be603ab04641993a94689cc51f09cc8e0370ba44720d68f4e6bda7f8642dfb0e1af9160bad319ecf4992f990065cd4bffdaddc7e27ab4dee597afc561202uwb)
    __builtin_abort ();
  if (bswap840 (0x4c23edce0b5ffd538da2112c333d17f100a0db44f7c9cfb097538e995f3a5bab5d487969a776bc3518cd614b0a783d0b18184f10e11ec078714f11d0896c7be6b5f54f5c6b0c9184d9f3cfe46b0cef84243e94055d547dcea38f9f2cfb1a13b00f620d7b6e9eff4891uwb)
      != 0x9148ff9e6e7b0d620fb0131afb2c9f8fa3ce7d545d05943e2484ef0c6be4cff3d984910c6b5c4ff5b5e67b6c89d0114f7178c01ee1104f18180b3d780a4b61cd1835bc76a76979485dab5b3a5f998e5397b0cfc9f744dba000f1173d332c11a28d53fd5f0bceed234cuwb)
    __builtin_abort ();
  if (bswap856 (0x41c39e2f987be9c81688ecc08a4ded319e4010952ecb6e4ac738a6924d81668f4404bedd7fa7a5952f9156bade43eb7d83fb12b2c780fc8eb9a22006238695678a53440afbe99f2a7af5e45938cfd6bccbe86e92696a20220c41282fc7be965211db5c0cc4fb373ff11b98uwb)
      != 0x981bf13f37fbc40c5cdb115296bec72f28410c22206a69926ee8cbbcd6cf3859e4f57a2a9fe9fb0a44538a679586230620a2b98efc80c7b212fb837deb43deba56912f95a5a77fddbe04448f66814d92a638c74a6ecb2e9510409e31ed4d8ac0ec8816c8e97b982f9ec341uwb)
    __builtin_abort ();
#endif
}
