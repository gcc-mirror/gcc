/* Test __builtin_bitreverseg.  */
/* { dg-do run { target { c || c++11 } } } */
/* { dg-options "-O2" } */

#if !__has_builtin(__builtin_bitreverseg)
#error __builtin_bitreverseg unsupported
#endif

#if __CHAR_BIT__ == 8
#if __SIZEOF_INT__ == 4
static_assert (__builtin_bitreverseg (0x2fa889a7U) == 0xe59115f4U, "");
#endif
#if __SIZEOF_LONG_LONG__ == 8
static_assert (__builtin_bitreverseg (0xd7632f428bdccb94ULL) == 0x29d33bd142f4c6ebULL, "");
#endif
static_assert (__builtin_bitreverseg ((unsigned char) 0xac) == (unsigned char) 0x35, "");
#if __SIZEOF_SHORT__ == 2
static_assert (__builtin_bitreverseg ((unsigned short) 0xc4f0) == (unsigned short) 0x0f23, "");
#endif
#if __SIZEOF_INT128__ == 16
static_assert (__builtin_bitreverseg ((((unsigned __int128) 0x398773ddb9250c54ULL) << 64)
				 | 0x26933639cdf63ea6ULL)
				 == ((((unsigned __int128) 0x657c6fb39c6cc964ULL) << 64)
				     | 0x2a30a49dbbcee19cULL), "");
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
static_assert (__builtin_bitreverseg (0xb75774uwb) == 0x2eeaeduwb, "");
static_assert (__builtin_bitreverseg (0xd55668c010uwb) == 0x0803166aabuwb, "");
static_assert (__builtin_bitreverseg ((unsigned _BitInt(129)) 0x01cc4426458663d6c35ad6f0c7117fc2b7uwb)
	       == 0x01da87fd11c61ed6b586d78cc344c84467uwb, "");
static_assert (__builtin_bitreverseg ((unsigned _BitInt(136)) 0xc73210faaad84376ebd396f82a166ac5f9uwb)
	       == 0x9fa35668541f69cbd76ec21b555f084ce3uwb, "");
static_assert (__builtin_bitreverseg ((unsigned _BitInt(832)) 0x09d2702b49ac2e9b1010df028451dceaf2cf9938035fa708921fb53a5d509d66220d916bb9bf07c9cfe6cc5437a83e2977d7617a360882c82838038588a0ecaaae7d16673d1d310c03fd603aa59f631c76c497adcc1975f451787ad9196684c7e39a2e20b75f2dbauwb)
	       == 0x5db4faed047459c7e32166989b5e1e8a2fae9833b5e9236e38c6f9a55c06bfc0308cb8bce668be7555370511a1c01c141341106c5e86ebee947c15ec2a3367f393e0fd9dd689b04466b90aba5cadf84910e5fac01c99f34f573b8a2140fb0808d9743592d40e4b90uwb, "");
static_assert (__builtin_bitreverseg ((unsigned _BitInt(833)) 0x0054078b08ef56720bb2a2cc52ddac983838de381c548c346e0b4274d51635f36b3c7e732bd4e536868702d864ae709ce74fd403a36037116c79854190bb34fbf7b26e238654590cdb5ce4400a54dcf1a3b1f546112c587da6ddbe3698f23190a49fb32af30c36cf68uwb)
	       == 0x002de6d8619ea99bf24a13189e32d8fb76cb7c346910c55f1b8b1e7654a0044e75b6613454c388ec9bdfbe59ba1305433c6d11d80d8b8057e5ce721cea4c3681c2c2d94e57a99cfc79ad9f58d1565c85a0ec5862547038f63838326b7694668a9ba09cd5ee21a3c054uwb, "");
static_assert (__builtin_bitreverseg ((unsigned _BitInt(856)) 0x6aa3d211688a1bc275204e2e83f7ecacfdc42aecdc1ae8093b901c207dc089e8645bf9cce6148e5b34dc89b7d47563d13a8ebd16a8a51fe3353b04b3fc8d9b60e9942ccfa8bb2adc97b4936b29f63c6384f9792c9f9810d4d41487d0a122308ab65c595e17843aaf38cd1auwb)
	       == 0x58b31cf55c21e87a9a3a6d510c44850be1282b2b0819f9349e9f21c63c6f94d6c92de93b54dd15f334299706d9b13fcd20dcacc7f8a51568bd715c8bc6ae2bed913b2cda712867339fda26179103be043809dc9017583b375423bf3537efc1747204ae43d85116884bc556uwb, "");
#endif

#if __CHAR_BIT__ == 8
#if __SIZEOF_INT__ == 4
[[gnu::noipa]] unsigned
bitreverse32 (unsigned x)
{
  return __builtin_bitreverseg (x);
}
#endif
#if __SIZEOF_LONG_LONG__ == 8
[[gnu::noipa]] unsigned long long
bitreverse64 (unsigned long long x)
{
  return __builtin_bitreverseg (x);
}
#endif
[[gnu::noipa]] unsigned char
bitreverse8 (unsigned char x)
{
  return __builtin_bitreverseg (x);
}
#if __SIZEOF_SHORT__ == 2
[[gnu::noipa]] unsigned short
bitreverse16 (unsigned short x)
{
  return __builtin_bitreverseg (x);
}
#endif
#if __SIZEOF_INT128__ == 16
[[gnu::noipa]] unsigned __int128
bitreverse128 (unsigned __int128 x)
{
  return __builtin_bitreverseg (x);
}
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
[[gnu::noipa]] unsigned _BitInt(24)
bitreverse24 (unsigned _BitInt(24) x)
{
  return __builtin_bitreverseg (x);
}

[[gnu::noipa]] unsigned _BitInt(40)
bitreverse40 (unsigned _BitInt(40) x)
{
  return __builtin_bitreverseg (x);
}

[[gnu::noipa]] unsigned _BitInt(129)
bitreverse129 (unsigned _BitInt(129) x)
{
  return __builtin_bitreverseg (x);
}

[[gnu::noipa]] unsigned _BitInt(136)
bitreverse136 (unsigned _BitInt(136) x)
{
  return __builtin_bitreverseg (x);
}

[[gnu::noipa]] unsigned _BitInt(832)
bitreverse832 (unsigned _BitInt(832) x)
{
  return __builtin_bitreverseg (x);
}

[[gnu::noipa]] unsigned _BitInt(833)
bitreverse833 (unsigned _BitInt(833) x)
{
  return __builtin_bitreverseg (x);
}

[[gnu::noipa]] unsigned _BitInt(856)
bitreverse856 (unsigned _BitInt(856) x)
{
  return __builtin_bitreverseg (x);
}
#endif

int
main ()
{
#if __CHAR_BIT__ == 8
#if __SIZEOF_INT__ == 4
  if (bitreverse32 (0x2fa889a7U) != 0xe59115f4U)
    __builtin_abort ();
#endif
#if __SIZEOF_LONG_LONG__ == 8
  if (bitreverse64 (0xd7632f428bdccb94ULL) != 0x29d33bd142f4c6ebULL)
    __builtin_abort ();
#endif
  if (bitreverse8 ((unsigned char) 0xac) != (unsigned char) 0x35)
    __builtin_abort ();
#if __SIZEOF_SHORT__ == 2
  if (bitreverse16 ((unsigned short) 0xc4f0) != (unsigned short) 0x0f23)
    __builtin_abort ();
#endif
#if __SIZEOF_INT128__ == 16
  if (bitreverse128 ((((unsigned __int128) 0x398773ddb9250c54ULL) << 64)
		| 0x26933639cdf63ea6ULL)
      != ((((unsigned __int128) 0x657c6fb39c6cc964ULL) << 64)
	  | 0x2a30a49dbbcee19cULL))
    __builtin_abort ();
#endif
#endif
#if __BITINT_MAXWIDTH__ >= 856
  if (bitreverse24 (0xb75774uwb) != 0x2eeaed)
    __builtin_abort ();
  if (bitreverse40 (0xd55668c010uwb) != 0x0803166aabuwb)
    __builtin_abort ();
  if (bitreverse129 (0x01cc4426458663d6c35ad6f0c7117fc2b7uwb)
	       != 0x01da87fd11c61ed6b586d78cc344c84467uwb)
    __builtin_abort ();
  if (bitreverse136 (0xc73210faaad84376ebd396f82a166ac5f9uwb)
      != 0x9fa35668541f69cbd76ec21b555f084ce3uwb)
    __builtin_abort ();
  if (bitreverse832 (0x09d2702b49ac2e9b1010df028451dceaf2cf9938035fa708921fb53a5d509d66220d916bb9bf07c9cfe6cc5437a83e2977d7617a360882c82838038588a0ecaaae7d16673d1d310c03fd603aa59f631c76c497adcc1975f451787ad9196684c7e39a2e20b75f2dbauwb)
      != 0x5db4faed047459c7e32166989b5e1e8a2fae9833b5e9236e38c6f9a55c06bfc0308cb8bce668be7555370511a1c01c141341106c5e86ebee947c15ec2a3367f393e0fd9dd689b04466b90aba5cadf84910e5fac01c99f34f573b8a2140fb0808d9743592d40e4b90uwb)
    __builtin_abort ();
  if (bitreverse833 (0x0054078b08ef56720bb2a2cc52ddac983838de381c548c346e0b4274d51635f36b3c7e732bd4e536868702d864ae709ce74fd403a36037116c79854190bb34fbf7b26e238654590cdb5ce4400a54dcf1a3b1f546112c587da6ddbe3698f23190a49fb32af30c36cf68uwb)
      != 0x002de6d8619ea99bf24a13189e32d8fb76cb7c346910c55f1b8b1e7654a0044e75b6613454c388ec9bdfbe59ba1305433c6d11d80d8b8057e5ce721cea4c3681c2c2d94e57a99cfc79ad9f58d1565c85a0ec5862547038f63838326b7694668a9ba09cd5ee21a3c054uwb)
    __builtin_abort ();
  if (bitreverse856 (0x6aa3d211688a1bc275204e2e83f7ecacfdc42aecdc1ae8093b901c207dc089e8645bf9cce6148e5b34dc89b7d47563d13a8ebd16a8a51fe3353b04b3fc8d9b60e9942ccfa8bb2adc97b4936b29f63c6384f9792c9f9810d4d41487d0a122308ab65c595e17843aaf38cd1auwb)
      != 0x58b31cf55c21e87a9a3a6d510c44850be1282b2b0819f9349e9f21c63c6f94d6c92de93b54dd15f334299706d9b13fcd20dcacc7f8a51568bd715c8bc6ae2bed913b2cda712867339fda26179103be043809dc9017583b375423bf3537efc1747204ae43d85116884bc556uwb)
    __builtin_abort ();
#endif
}
