/* Test __builtin_clmulg.  */
/* { dg-do run { target { c || c++11 } } } */
/* { dg-options "-O2" } */

#if !__has_builtin(__builtin_clmulg)
#error __builtin_clmulg unsupported
#endif

#if __CHAR_BIT__ == 8
static_assert (__builtin_clmulg ((unsigned char) 0x7e, (unsigned char) 0x9c) == 0xe8, "");
#if __SIZEOF_SHORT__ == 2
static_assert (__builtin_clmulg ((unsigned short) 0xc8aa, (unsigned short) 0x7616) == 0xa55c, "");
#endif
#if __SIZEOF_INT__ == 4
static_assert (__builtin_clmulg (0x9f330680U, 0x4e157eadU) == 0xb23fb880U, "");
static_assert (__builtin_clmulg (0x11111111U, 0x11111111U) == 0x1010101U, "");
static_assert (__builtin_clmulg (~0U, ~0U) == 0x55555555U, "");
#endif
#if __SIZEOF_LONG__ == 4
static_assert (__builtin_clmulg (0xcd63e2c1UL, 0xab176a37UL) == 0x2f053277UL, "");
#elif __SIZEOF_LONG__ == 8
static_assert (__builtin_clmulg (0x43e13a77bec666afUL, 0x39c165e2ed3f98f7UL) == 0xe8465657c87e7a1dUL, "");
#endif
#if __SIZEOF_LONG_LONG__ == 8
static_assert (__builtin_clmulg (0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL) == 0xfaf0ddab7a75024ULL, "");
static_assert (__builtin_clmulg (0x11111111ULL, 0x11111111ULL) == 0x101010101010101ULL, "");
static_assert (__builtin_clmulg (0x1111111111111111ULL, 0x1111111111111111ULL) == 0x101010101010101ULL, "");
static_assert (__builtin_clmulg (~0ULL, ~0ULL) == 0x5555555555555555ULL, "");
#endif
#if __SIZEOF_INT128__ == 16
static_assert (__builtin_clmulg ((((unsigned __int128) 0x983a442a106b8a83ULL) << 64)
				 | 0x1866ccfa2e2b382cULL,
				 (((unsigned __int128) 0xe9abf4d856d535c3ULL) << 64)
				 | 0x58708f117a52c14cULL)
				 == ((((unsigned __int128) 0xd3ae65b0bac566b2ULL) << 64)
				     | 0xdd6b33f9bb8606d0ULL), "");
#endif
#endif

#if __BITINT_MAXWIDTH__ >= 856
static_assert (__builtin_clmulg ((unsigned _BitInt(24)) 0xea754cuwb,
				 (unsigned _BitInt(24)) 0x7cfcb9uwb) == 0xd946cuwb, "");
static_assert (__builtin_clmulg ((unsigned _BitInt(40)) 0xb35cea0986uwb,
				 (unsigned _BitInt(40)) 0xec8cec0cffuwb) == 0xcf45a35282uwb, "");
static_assert (__builtin_clmulg ((unsigned _BitInt(128)) 0x40a6e26b30f2ff0e2b099e7a71429d54uwb,
				 (unsigned _BitInt(128)) 0xddd32528056500d0b79218aa8b55046cuwb)
	       == 0xa11e12e273ecbf7d7f0c6f8409db3070uwb, "");
static_assert (__builtin_clmulg ((unsigned _BitInt(129)) 0x01a057b67e5807da3e87d86e34ead6d446uwb,
				 (unsigned _BitInt(129)) 0x00ebfe8616a19a74e093c39217b1f599c5uwb)
	       == 0x7aa05ee8e53876da9b60fbe653da1deuwb, "");
static_assert (__builtin_clmulg ((unsigned _BitInt(136)) 0xe8654afad921062835d589e9de710c59e2uwb,
				 (unsigned _BitInt(136)) 0xec973d421e8e06fda1fc1ec83be74e7ac5uwb)
	       == 0xcaba6f2d4948b5e794137742285f7d83eauwb, "");
static_assert (__builtin_clmulg ((unsigned _BitInt(832)) 0x3bf0e6fcd0acb795c68d436f4038d14f1eeadd2447157b04d15ad94ec0638ffb5375f72321afb8e73cfb577d3328cc5213a9765abff15e904c37de0c9b6d08eee2ff1204aecaebebc64268f96b344b7edec2d89db3362dff6e0b0c097814f75a13095ec2d44aad9auwb,
				 (unsigned _BitInt(832)) 0x527f6addf01c1827f833ac6222d6d1c2b114862834006297b3a44b8c445a2b97da9574cab18cf1a9c09d0be273dca424f12b4c252baebdde52086b9763962e3d2ba207dd2ef886ee9592d1086e752c5fa07885cc2742aa794a1510adab3eead7e0f1b40fe93afd7euwb)
	       == 0xed1958f12923520d74fc66cea0973580825a5e563806308c9740731d7873df8d1842a1e78f7c2d957aa122477042739466df964d826d3ae2b77b0d153ba77375b4d53f85667ee9e99bc6e4f20e15ede19ea949c1c3783205003ac0dca600daa9cfc51777640ccfecuwb, "");
static_assert (__builtin_clmulg ((unsigned _BitInt(856)) 0x7118148ab2f165085830aa8b252e2d1600b3264181b7b611c72bce4e05cb0276e316019607669e609748ebbc7618d276ccf8b84db06e5f77992dc59ef8c715dcdd1672e57c1045135830d0ce49a245159bfd624b6bc1c204ef88a2e74fb7c32dcd35124a45575d9d872d6cuwb,
				 (unsigned _BitInt(856)) 0x04c996a2400c474c71d73fb208c9f256676d0beebc3dd80e1dfce8d3cb7613d03faa7280b6b9cc27900bd998d4cbee3b38fa2af537020355feeb28ca613b9aa1e50c219bc5edc255f89bedcc67db079fd531940d33976232838afce4c59685aba2a646679309bc8ba4a957uwb)
	       == 0xa969732632882314035c08bb15c392653af107f06b90b4e63580f503c7be7163bb379407420d9b72f0147d9a2c4008ed148b88a6c4304c923ec6e148484572893b3474829536e0a44a0400d10764cb7538ef2c24cd7d18374bbead68028cca6cc3433c6ce6f679de41c3c4uwb, "");
#endif

[[gnu::noipa]] unsigned char
clmul8 (unsigned char x, unsigned char y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned char
clmul8w (unsigned char x, unsigned char y, unsigned char *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned char
clmul8b (bool x, bool y)
{
  return __builtin_clmulg ((unsigned char) x, (unsigned char) y);
}

[[gnu::noipa]] unsigned char
clmul8m (unsigned char x, unsigned char y)
{
  return __builtin_clmulg (x, (unsigned char) (y & 0x3f));
}

[[gnu::noipa]] unsigned short
clmul16 (unsigned short x, unsigned short y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned short
clmul16w (unsigned short x, unsigned short y, unsigned short *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned int
clmul32 (unsigned int x, unsigned int y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned int
clmul32w (unsigned int x, unsigned int y, unsigned int *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned int
clmul32m (unsigned int x, unsigned int y)
{
  return __builtin_clmulg (x & 0x1fffffU, y);
}

[[gnu::noipa]] unsigned int
clmul32m2 (unsigned int x, unsigned int y)
{
  return __builtin_clmulg (x & 0x1fU, y & 0xf);
}

[[gnu::noipa]] unsigned int
clmul32m3 (unsigned int x, unsigned int y)
{
  return __builtin_clmulg (x & 0x7fffU, y & 0x3ffU);
}

[[gnu::noipa]] unsigned long
clmull (unsigned long x, unsigned long y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned long
clmullw (unsigned long x, unsigned long y, unsigned long *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned long long
clmul64 (unsigned long long x, unsigned long long y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned long long
clmul64w (unsigned long long x, unsigned long long y, unsigned long long *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned long long
clmul64wm (unsigned long long x, unsigned long long y, unsigned long long *z)
{
  return __builtin_clmulg (x & 0x3fffffffULL, y & 0x3fffffffULL, z);
}

#if __CHAR_BIT__ == 8 && __SIZEOF_INT128__ == 16
[[gnu::noipa]] unsigned __int128
clmul128 (unsigned __int128 x, unsigned __int128 y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned __int128
clmul128w (unsigned __int128 x, unsigned __int128 y, unsigned __int128 *z)
{
  return __builtin_clmulg (x, y, z);
}
#endif

#if __BITINT_MAXWIDTH__ >= 856
[[gnu::noipa]] unsigned _BitInt(1)
clmul1 (unsigned _BitInt(1) x, unsigned _BitInt(1) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(1)
clmul1w (unsigned _BitInt(1) x, unsigned _BitInt(1) y, unsigned _BitInt(1) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(24)
clmul24 (unsigned _BitInt(24) x, unsigned _BitInt(24) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(24)
clmul24w (unsigned _BitInt(24) x, unsigned _BitInt(24) y, unsigned _BitInt(24) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(40)
clmul40 (unsigned _BitInt(40) x, unsigned _BitInt(40) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(40)
clmul40w (unsigned _BitInt(40) x, unsigned _BitInt(40) y, unsigned _BitInt(40) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(128)
clmul128b (unsigned _BitInt(128) x, unsigned _BitInt(128) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(128)
clmul128bw (unsigned _BitInt(128) x, unsigned _BitInt(128) y, unsigned _BitInt(128) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(129)
clmul129 (unsigned _BitInt(129) x, unsigned _BitInt(129) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(129)
clmul129w (unsigned _BitInt(129) x, unsigned _BitInt(129) y, unsigned _BitInt(129) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(136)
clmul136 (unsigned _BitInt(136) x, unsigned _BitInt(136) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(136)
clmul136w (unsigned _BitInt(136) x, unsigned _BitInt(136) y, unsigned _BitInt(136) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(832)
clmul832 (unsigned _BitInt(832) x, unsigned _BitInt(832) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(832)
clmul832w (unsigned _BitInt(832) x, unsigned _BitInt(832) y, unsigned _BitInt(832) *z)
{
  return __builtin_clmulg (x, y, z);
}

[[gnu::noipa]] unsigned _BitInt(856)
clmul856 (unsigned _BitInt(856) x, unsigned _BitInt(856) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(856)
clmul856w (unsigned _BitInt(856) x, unsigned _BitInt(856) y, unsigned _BitInt(856) *z)
{
  return __builtin_clmulg (x, y, z);
}
#endif

#if __BITINT_MAXWIDTH__ >= 65535
[[gnu::noipa]] unsigned _BitInt(65535)
clmul65535 (unsigned _BitInt(65535) x, unsigned _BitInt(65535) y)
{
  return __builtin_clmulg (x, y);
}

[[gnu::noipa]] unsigned _BitInt(65535)
clmul65535w (unsigned _BitInt(65535) x, unsigned _BitInt(65535) y, unsigned _BitInt(65535) *z)
{
  return __builtin_clmulg (x, y, z);
}
#endif

int
main ()
{
#if __CHAR_BIT__ == 8
  if (clmul8 ((unsigned char) 0x7e, (unsigned char) 0x9c) != 0xe8)
    __builtin_abort ();
  if (clmul8b (false, false) != 0
      || clmul8b (false, true) != 0
      || clmul8b (true, false) != 0
      || clmul8b (true, true) != 1)
    __builtin_abort ();
  if (clmul8m ((unsigned char) 0x7e, (unsigned char) 0x9c)
      != __builtin_clmulg ((unsigned char) 0x7e, (unsigned char) (0x9c & 0x3f)))
    __builtin_abort ();
  if (clmul8m ((unsigned char) 0xff, (unsigned char) 0xff)
      != __builtin_clmulg ((unsigned char) 0xff, (unsigned char) (0xff & 0x3f)))
    __builtin_abort ();
  {
    unsigned char hi;
    if (clmul8w ((unsigned char) 0x7e, (unsigned char) 0x9c, &hi) != 0xe8
	|| hi != 0x3a)
      __builtin_abort ();
  }
#if __SIZEOF_SHORT__ == 2
  if (clmul16 ((unsigned short) 0xc8aa, (unsigned short) 0x7616) != 0xa55c)
    __builtin_abort ();
  {
    unsigned short hi;
    if (clmul16w ((unsigned short) 0xc8aa, (unsigned short) 0x7616, &hi) != 0xa55c
	|| hi != 0x2508)
      __builtin_abort ();
  }
#endif
#if __SIZEOF_INT__ == 4
  if (clmul32 (0x9f330680U, 0x4e157eadU) != 0xb23fb880U)
    __builtin_abort ();
  if (clmul32 (0x11111111U, 0x11111111U) != 0x1010101U)
    __builtin_abort ();
  if (clmul32 (~0U, ~0U) != 0x55555555U)
    __builtin_abort ();
  if (clmul32m (0x9f330680U, 0x4e157eadU)
      != __builtin_clmulg (0x9f330680U & 0x1fffffU, 0x4e157eadU))
    __builtin_abort ();
  if (clmul32m2 (0x9f330680U, 0x4e157eadU)
      != __builtin_clmulg (0x9f330680U & 0x1fU, 0x4e157eadU & 0xfU))
    __builtin_abort ();
  if (clmul32m3 (0x9f330680U, 0x4e157eadU)
      != __builtin_clmulg (0x9f330680U & 0x7fffU, 0x4e157eadU & 0x3ffU))
    __builtin_abort ();
  if (clmul32m (~0U, ~0U) != __builtin_clmulg (0x1fffffU, ~0U))
    __builtin_abort ();
  if (clmul32m2 (~0U, ~0U) != __builtin_clmulg (0x1fU, 0xfU))
    __builtin_abort ();
  if (clmul32m3 (~0U, ~0U) != __builtin_clmulg (0x7fffU, 0x3ffU))
    __builtin_abort ();
  {
    unsigned int hi;
    if (clmul32w (0x9f330680U, 0x4e157eadU, &hi) != 0xb23fb880U
	|| hi != 0x207cd974)
      __builtin_abort ();
    if (clmul32w (0x11111111U, 0x11111111U, &hi) != 0x1010101U
	|| hi != 0x1010101U)
      __builtin_abort ();
    if (clmul32w (~0U, ~0U, &hi) != 0x55555555U
	|| hi != 0x55555555U)
      __builtin_abort ();
  }
#endif
#if __SIZEOF_LONG__ == 4
  if (clmull (0xcd63e2c1UL, 0xab176a37UL) != 0x2f053277UL)
    __builtin_abort ();
  {
    unsigned long hi;
    if (clmullw (0xcd63e2c1UL, 0xab176a37UL, &hi) != 0x2f053277UL
	|| hi != 0x782faf0eUL)
      __builtin_abort ();
  }
#elif __SIZEOF_LONG__ == 8
  if (clmull (0x43e13a77bec666afUL, 0x39c165e2ed3f98f7UL) != 0xe8465657c87e7a1dUL)
    __builtin_abort ();
  {
    unsigned long hi;
    if (clmullw (0x43e13a77bec666afUL, 0x39c165e2ed3f98f7UL, &hi) != 0xe8465657c87e7a1dUL
	|| hi != 0xe2f8ec31f25371bUL)
      __builtin_abort ();
  }
#endif
#if __SIZEOF_LONG_LONG__ == 8
  if (clmul64 (0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL) != 0xfaf0ddab7a75024ULL)
    __builtin_abort ();
  if (clmul64 (0x11111111ULL, 0x11111111ULL) != 0x101010101010101ULL)
    __builtin_abort ();
  if (clmul64 (0x1111111111111111ULL, 0x1111111111111111ULL) != 0x101010101010101ULL)
    __builtin_abort ();
  if (clmul64 (~0ULL, ~0ULL) != 0x5555555555555555ULL)
    __builtin_abort ();
  {
    unsigned long long hi;
    if (clmul64w (0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL, &hi) != 0xfaf0ddab7a75024ULL
	|| hi != 0x29daaa7ee7cd2860ULL)
      __builtin_abort ();
    if (clmul64w (0x11111111ULL, 0x11111111ULL, &hi) != 0x101010101010101ULL
	|| hi != 0ULL)
      __builtin_abort ();
    if (clmul64w (0x1111111111111111ULL, 0x1111111111111111ULL, &hi) != 0x101010101010101ULL
	|| hi != 0x101010101010101ULL)
      __builtin_abort ();
    if (clmul64w (~0ULL, ~0ULL, &hi) != 0x5555555555555555ULL
	|| hi != 0x5555555555555555ULL)
      __builtin_abort ();
    if (clmul64wm (0xe38ebed3ef3cc8deULL, 0x75224424100f8096ULL, &hi)
	!= __builtin_clmulg (0xe38ebed3ef3cc8deULL & 0x3fffffffULL,
			     0x75224424100f8096ULL & 0x3fffffffULL)
	|| hi != 0ULL)
      __builtin_abort ();
    if (clmul64wm (~0ULL, ~0ULL, &hi) != __builtin_clmulg (0x3fffffffULL, 0x3fffffffULL)
	|| hi != 0ULL)
      __builtin_abort ();
  }
#endif
#if __SIZEOF_INT128__ == 16
  if (clmul128 ((((unsigned __int128) 0x983a442a106b8a83ULL) << 64)
		| 0x1866ccfa2e2b382cULL,
		(((unsigned __int128) 0xe9abf4d856d535c3ULL) << 64)
		| 0x58708f117a52c14cULL)
      != ((((unsigned __int128) 0xd3ae65b0bac566b2ULL) << 64)
	  | 0xdd6b33f9bb8606d0ULL))
    __builtin_abort ();
  {
    unsigned __int128 hi;
    if (clmul128w ((((unsigned __int128) 0x983a442a106b8a83ULL) << 64)
		   | 0x1866ccfa2e2b382cULL,
		   (((unsigned __int128) 0xe9abf4d856d535c3ULL) << 64)
		   | 0x58708f117a52c14cULL, &hi)
	!= ((((unsigned __int128) 0xd3ae65b0bac566b2ULL) << 64)
	    | 0xdd6b33f9bb8606d0ULL)
	|| hi != ((((unsigned __int128) 0x7d17129902648920ULL) << 64)
		  | 0xfdb34e3b7c058047ULL))
    __builtin_abort ();
  }
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
  if (clmul1 (0, 0) != 0
      || clmul1 (0, 1) != 0
      || clmul1 (1, 0) != 0
      || clmul1 (1, 1) != 1)
    __builtin_abort ();
  if (clmul24 (0xea754cuwb, 0x7cfcb9uwb) != 0xd946cuwb)
    __builtin_abort ();
  if (clmul40 (0xb35cea0986uwb, 0xec8cec0cffuwb) != 0xcf45a35282uwb)
    __builtin_abort ();
  if (clmul128b (0x40a6e26b30f2ff0e2b099e7a71429d54uwb, 0xddd32528056500d0b79218aa8b55046cuwb)
      != 0xa11e12e273ecbf7d7f0c6f8409db3070uwb)
    __builtin_abort ();
  if (clmul129 (0x01a057b67e5807da3e87d86e34ead6d446uwb, 0x00ebfe8616a19a74e093c39217b1f599c5uwb)
      != 0x7aa05ee8e53876da9b60fbe653da1deuwb)
    __builtin_abort ();
  if (clmul136 (0xe8654afad921062835d589e9de710c59e2uwb, 0xec973d421e8e06fda1fc1ec83be74e7ac5uwb)
      != 0xcaba6f2d4948b5e794137742285f7d83eauwb)
    __builtin_abort ();
  if (clmul832 (0x3bf0e6fcd0acb795c68d436f4038d14f1eeadd2447157b04d15ad94ec0638ffb5375f72321afb8e73cfb577d3328cc5213a9765abff15e904c37de0c9b6d08eee2ff1204aecaebebc64268f96b344b7edec2d89db3362dff6e0b0c097814f75a13095ec2d44aad9auwb,
		0x527f6addf01c1827f833ac6222d6d1c2b114862834006297b3a44b8c445a2b97da9574cab18cf1a9c09d0be273dca424f12b4c252baebdde52086b9763962e3d2ba207dd2ef886ee9592d1086e752c5fa07885cc2742aa794a1510adab3eead7e0f1b40fe93afd7euwb)
      != 0xed1958f12923520d74fc66cea0973580825a5e563806308c9740731d7873df8d1842a1e78f7c2d957aa122477042739466df964d826d3ae2b77b0d153ba77375b4d53f85667ee9e99bc6e4f20e15ede19ea949c1c3783205003ac0dca600daa9cfc51777640ccfecuwb)
    __builtin_abort ();
  if (clmul856 (0x7118148ab2f165085830aa8b252e2d1600b3264181b7b611c72bce4e05cb0276e316019607669e609748ebbc7618d276ccf8b84db06e5f77992dc59ef8c715dcdd1672e57c1045135830d0ce49a245159bfd624b6bc1c204ef88a2e74fb7c32dcd35124a45575d9d872d6cuwb,
		0x04c996a2400c474c71d73fb208c9f256676d0beebc3dd80e1dfce8d3cb7613d03faa7280b6b9cc27900bd998d4cbee3b38fa2af537020355feeb28ca613b9aa1e50c219bc5edc255f89bedcc67db079fd531940d33976232838afce4c59685aba2a646679309bc8ba4a957uwb)
      != 0xa969732632882314035c08bb15c392653af107f06b90b4e63580f503c7be7163bb379407420d9b72f0147d9a2c4008ed148b88a6c4304c923ec6e148484572893b3474829536e0a44a0400d10764cb7538ef2c24cd7d18374bbead68028cca6cc3433c6ce6f679de41c3c4uwb)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 856 * 2
  {
    unsigned _BitInt(1) hi;
    if (clmul1w (0, 0, &hi) != 0
	|| hi != 0
	|| clmul1w (0, 1, &hi) != 0
	|| hi != 0
	|| clmul1w (1, 0, &hi) != 0
	|| hi != 0
	|| clmul1w (1, 1, &hi) != 1
	|| hi != 0)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(24) hi;
    if (clmul24w (0xea754cuwb, 0x7cfcb9uwb, &hi) != 0xd946cuwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(24 * 2)) 0xea754cuwb,
				   (unsigned _BitInt(24 * 2)) 0x7cfcb9uwb) >> 24)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(40) hi;
    if (clmul40w (0xb35cea0986uwb, 0xec8cec0cffuwb, &hi) != 0xcf45a35282uwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(40 * 2)) 0xb35cea0986uwb,
				   (unsigned _BitInt(40 * 2)) 0xec8cec0cffuwb) >> 40)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(128) hi;
    if (clmul128bw (0x40a6e26b30f2ff0e2b099e7a71429d54uwb, 0xddd32528056500d0b79218aa8b55046cuwb, &hi)
	!= 0xa11e12e273ecbf7d7f0c6f8409db3070uwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(128 * 2)) 0x40a6e26b30f2ff0e2b099e7a71429d54uwb,
				   (unsigned _BitInt(128 * 2)) 0xddd32528056500d0b79218aa8b55046cuwb) >> 128)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(129) hi;
    if (clmul129w (0x01a057b67e5807da3e87d86e34ead6d446uwb, 0x00ebfe8616a19a74e093c39217b1f599c5uwb, &hi)
	!= 0x7aa05ee8e53876da9b60fbe653da1deuwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(129 * 2)) 0x01a057b67e5807da3e87d86e34ead6d446uwb,
				   (unsigned _BitInt(129 * 2)) 0x00ebfe8616a19a74e093c39217b1f599c5uwb) >> 129)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(136) hi;
    if (clmul136w (0xe8654afad921062835d589e9de710c59e2uwb, 0xec973d421e8e06fda1fc1ec83be74e7ac5uwb, &hi)
	!= 0xcaba6f2d4948b5e794137742285f7d83eauwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(136 * 2)) 0xe8654afad921062835d589e9de710c59e2uwb,
				   (unsigned _BitInt(136 * 2)) 0xec973d421e8e06fda1fc1ec83be74e7ac5uwb) >> 136)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(832) hi;
    if (clmul832w (0x3bf0e6fcd0acb795c68d436f4038d14f1eeadd2447157b04d15ad94ec0638ffb5375f72321afb8e73cfb577d3328cc5213a9765abff15e904c37de0c9b6d08eee2ff1204aecaebebc64268f96b344b7edec2d89db3362dff6e0b0c097814f75a13095ec2d44aad9auwb,
		   0x527f6addf01c1827f833ac6222d6d1c2b114862834006297b3a44b8c445a2b97da9574cab18cf1a9c09d0be273dca424f12b4c252baebdde52086b9763962e3d2ba207dd2ef886ee9592d1086e752c5fa07885cc2742aa794a1510adab3eead7e0f1b40fe93afd7euwb, &hi)
	!= 0xed1958f12923520d74fc66cea0973580825a5e563806308c9740731d7873df8d1842a1e78f7c2d957aa122477042739466df964d826d3ae2b77b0d153ba77375b4d53f85667ee9e99bc6e4f20e15ede19ea949c1c3783205003ac0dca600daa9cfc51777640ccfecuwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(832 * 2)) 0x3bf0e6fcd0acb795c68d436f4038d14f1eeadd2447157b04d15ad94ec0638ffb5375f72321afb8e73cfb577d3328cc5213a9765abff15e904c37de0c9b6d08eee2ff1204aecaebebc64268f96b344b7edec2d89db3362dff6e0b0c097814f75a13095ec2d44aad9auwb,
				   (unsigned _BitInt(832 * 2)) 0x527f6addf01c1827f833ac6222d6d1c2b114862834006297b3a44b8c445a2b97da9574cab18cf1a9c09d0be273dca424f12b4c252baebdde52086b9763962e3d2ba207dd2ef886ee9592d1086e752c5fa07885cc2742aa794a1510adab3eead7e0f1b40fe93afd7euwb) >> 832)
      __builtin_abort ();
  }
  {
    unsigned _BitInt(856) hi;
    if (clmul856w (0x7118148ab2f165085830aa8b252e2d1600b3264181b7b611c72bce4e05cb0276e316019607669e609748ebbc7618d276ccf8b84db06e5f77992dc59ef8c715dcdd1672e57c1045135830d0ce49a245159bfd624b6bc1c204ef88a2e74fb7c32dcd35124a45575d9d872d6cuwb,
		   0x04c996a2400c474c71d73fb208c9f256676d0beebc3dd80e1dfce8d3cb7613d03faa7280b6b9cc27900bd998d4cbee3b38fa2af537020355feeb28ca613b9aa1e50c219bc5edc255f89bedcc67db079fd531940d33976232838afce4c59685aba2a646679309bc8ba4a957uwb, &hi)
	!= 0xa969732632882314035c08bb15c392653af107f06b90b4e63580f503c7be7163bb379407420d9b72f0147d9a2c4008ed148b88a6c4304c923ec6e148484572893b3474829536e0a44a0400d10764cb7538ef2c24cd7d18374bbead68028cca6cc3433c6ce6f679de41c3c4uwb
	|| hi != __builtin_clmulg ((unsigned _BitInt(856 * 2)) 0x7118148ab2f165085830aa8b252e2d1600b3264181b7b611c72bce4e05cb0276e316019607669e609748ebbc7618d276ccf8b84db06e5f77992dc59ef8c715dcdd1672e57c1045135830d0ce49a245159bfd624b6bc1c204ef88a2e74fb7c32dcd35124a45575d9d872d6cuwb,
				   (unsigned _BitInt(856 * 2)) 0x04c996a2400c474c71d73fb208c9f256676d0beebc3dd80e1dfce8d3cb7613d03faa7280b6b9cc27900bd998d4cbee3b38fa2af537020355feeb28ca613b9aa1e50c219bc5edc255f89bedcc67db079fd531940d33976232838afce4c59685aba2a646679309bc8ba4a957uwb) >> 856)
      __builtin_abort ();
  }
#endif
}
