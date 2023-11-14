/* PR c/111309 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
clzc (unsigned char x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzc2 (unsigned char x, int y)
{
  return __builtin_clzg (x, y);
}

__attribute__((noipa)) int
clzs (unsigned short x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzs2 (unsigned short x)
{
  return __builtin_clzg (x, -2);
}

__attribute__((noipa)) int
clzi (unsigned int x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzi2 (unsigned int x, int y)
{
  return __builtin_clzg (x, y);
}

__attribute__((noipa)) int
clzl (unsigned long x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzl2 (unsigned long x)
{
  return __builtin_clzg (x, -1);
}

__attribute__((noipa)) int
clzL (unsigned long long x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzL2 (unsigned long long x, int y)
{
  return __builtin_clzg (x, y);
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) int
clzI (unsigned __int128 x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzI2 (unsigned __int128 x, int y)
{
  return __builtin_clzg (x, y);
}
#endif

__attribute__((noipa)) int
ctzc (unsigned char x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzc2 (unsigned char x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
ctzs (unsigned short x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzs2 (unsigned short x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
ctzi (unsigned int x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzi2 (unsigned int x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
ctzl (unsigned long x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzl2 (unsigned long x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
ctzL (unsigned long long x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzL2 (unsigned long long x, int y)
{
  return __builtin_ctzg (x, y);
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) int
ctzI (unsigned __int128 x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzI2 (unsigned __int128 x)
{
  return __builtin_ctzg (x, __SIZEOF_INT128__ * __CHAR_BIT__);
}
#endif

__attribute__((noipa)) int
clrsbc (signed char x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
clrsbs (signed short x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
clrsbi (signed int x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
clrsbl (signed long x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
clrsbL (signed long long x)
{
  return __builtin_clrsbg (x);
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) int
clrsbI (signed __int128 x)
{
  return __builtin_clrsbg (x);
}
#endif

__attribute__((noipa)) int
ffsc (signed char x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
ffss (signed short x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
ffsi (signed int x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
ffsl (signed long x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
ffsL (signed long long x)
{
  return __builtin_ffsg (x);
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) int
ffsI (signed __int128 x)
{
  return __builtin_ffsg (x);
}
#endif

__attribute__((noipa)) int
parityc (unsigned char x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
paritys (unsigned short x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
parityi (unsigned int x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
parityl (unsigned long x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
parityL (unsigned long long x)
{
  return __builtin_parityg (x);
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) int
parityI (unsigned __int128 x)
{
  return __builtin_parityg (x);
}
#endif

__attribute__((noipa)) int
popcountc (unsigned char x)
{
  return __builtin_popcountg (x);
}

__attribute__((noipa)) int
popcounts (unsigned short x)
{
  return __builtin_popcountg (x);
}

__attribute__((noipa)) int
popcounti (unsigned int x)
{
  return __builtin_popcountg (x);
}

__attribute__((noipa)) int
popcountl (unsigned long x)
{
  return __builtin_popcountg (x);
}

__attribute__((noipa)) int
popcountL (unsigned long long x)
{
  return __builtin_popcountg (x);
}

#ifdef __SIZEOF_INT128__
__attribute__((noipa)) int
popcountI (unsigned __int128 x)
{
  return __builtin_popcountg (x);
}
#endif

int
main ()
{
  if (__builtin_clzg ((unsigned char) 1) != __CHAR_BIT__ - 1
      || __builtin_clzg ((unsigned short) 2, -2) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 2
      || __builtin_clzg (0U, 42) != 42
      || __builtin_clzg (0U, -1) != -1
      || __builtin_clzg (1U) != __SIZEOF_INT__ * __CHAR_BIT__ - 1
      || __builtin_clzg (2UL, -1) != __SIZEOF_LONG__ * __CHAR_BIT__ - 2
      || __builtin_clzg (5ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 3
#ifdef __SIZEOF_INT128__
      || __builtin_clzg ((unsigned __int128) 9) != __SIZEOF_INT128__ * __CHAR_BIT__ - 4
#endif
      || __builtin_clzg (~0U, -5) != 0
      || __builtin_clzg (~0ULL >> 2) != 2
      || __builtin_ctzg ((unsigned char) 1) != 0
      || __builtin_ctzg ((unsigned short) 28) != 2
      || __builtin_ctzg (0U, 32) != 32
      || __builtin_ctzg (0U, -42) != -42
      || __builtin_ctzg (1U) != 0
      || __builtin_ctzg (16UL, -1) != 4
      || __builtin_ctzg (5ULL << 52, 0) != 52
#ifdef __SIZEOF_INT128__
      || __builtin_ctzg (((unsigned __int128) 9) << 72) != 72
#endif
      || __builtin_clrsbg ((signed char) 0) != __CHAR_BIT__ - 1
      || __builtin_clrsbg ((signed short) -1) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 1
      || __builtin_clrsbg (0) != __SIZEOF_INT__ * __CHAR_BIT__ - 1
      || __builtin_clrsbg (-1L) != __SIZEOF_LONG__ * __CHAR_BIT__ - 1
      || __builtin_clrsbg (0LL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1
#ifdef __SIZEOF_INT128__
      || __builtin_clrsbg ((__int128) -1) != __SIZEOF_INT128__ * __CHAR_BIT__ - 1
#endif
      || __builtin_clrsbg (0x1afb) != __SIZEOF_INT__ * __CHAR_BIT__ - 14
      || __builtin_clrsbg (-2) != __SIZEOF_INT__ * __CHAR_BIT__ - 2
      || __builtin_clrsbg (1L) != __SIZEOF_LONG__ * __CHAR_BIT__ - 2
      || __builtin_clrsbg (-4LL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 3
      || __builtin_ffsg ((signed char) 0) != 0
      || __builtin_ffsg ((signed short) 0) != 0
      || __builtin_ffsg (0) != 0
      || __builtin_ffsg (0L) != 0
      || __builtin_ffsg (0LL) != 0
#ifdef __SIZEOF_INT128__
      || __builtin_ffsg ((__int128) 0) != 0
#endif
      || __builtin_ffsg ((signed char) 4) != 3
      || __builtin_ffsg ((signed short) 8) != 4
      || __builtin_ffsg (1) != 1
      || __builtin_ffsg (2L) != 2
      || __builtin_ffsg (28LL) != 3
      || __builtin_parityg ((unsigned char) 1) != 1
      || __builtin_parityg ((unsigned short) 2) != 1
      || __builtin_parityg (0U) != 0
      || __builtin_parityg (3U) != 0
      || __builtin_parityg (0UL) != 0
      || __builtin_parityg (7UL) != 1
      || __builtin_parityg (0ULL) != 0
#ifdef __SIZEOF_INT128__
      || __builtin_parityg ((unsigned __int128) 0) != 0
#endif
      || __builtin_parityg ((unsigned char) ~0U) != 0
      || __builtin_parityg ((unsigned short) ~0U) != 0
      || __builtin_parityg (~0U) != 0
      || __builtin_parityg (~0UL) != 0
      || __builtin_parityg (~0ULL) != 0
#ifdef __SIZEOF_INT128__
      || __builtin_parityg (~(unsigned __int128) 0) != 0
#endif
      || __builtin_popcountg (0U) != 0
      || __builtin_popcountg (0UL) != 0
      || __builtin_popcountg (0ULL) != 0
#ifdef __SIZEOF_INT128__
      || __builtin_popcountg ((unsigned __int128) 0) != 0
#endif
      || __builtin_popcountg ((unsigned char) ~0U) != __CHAR_BIT__
      || __builtin_popcountg ((unsigned short) ~0U) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || __builtin_popcountg (~0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || __builtin_popcountg (~0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || __builtin_popcountg (~0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__
#ifdef __SIZEOF_INT128__
      || __builtin_popcountg (~(unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__
#endif
      || 0)
  __builtin_abort ();
  if (clzc (1) != __CHAR_BIT__ - 1
      || clzs2 (2) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 2
      || clzi2 (0U, 42) != 42
      || clzi2 (0U, -1) != -1
      || clzi (1U) != __SIZEOF_INT__ * __CHAR_BIT__ - 1
      || clzl2 (2UL) != __SIZEOF_LONG__ * __CHAR_BIT__ - 2
      || clzL (5ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 3
#ifdef __SIZEOF_INT128__
      || clzI ((unsigned __int128) 9) != __SIZEOF_INT128__ * __CHAR_BIT__ - 4
#endif
      || clzi2 (~0U, -5) != 0
      || clzL (~0ULL >> 2) != 2
      || ctzc (1) != 0
      || ctzs (28) != 2
      || ctzi2 (0U, 32) != 32
      || ctzi2 (0U, -42) != -42
      || ctzi (1U) != 0
      || ctzl2 (16UL, -1) != 4
      || ctzL2 (5ULL << 52, 0) != 52
#ifdef __SIZEOF_INT128__
      || ctzI (((unsigned __int128) 9) << 72) != 72
#endif
      || clrsbc (0) != __CHAR_BIT__ - 1
      || clrsbs (-1) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 1
      || clrsbi (0) != __SIZEOF_INT__ * __CHAR_BIT__ - 1
      || clrsbl (-1L) != __SIZEOF_LONG__ * __CHAR_BIT__ - 1
      || clrsbL (0LL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1
#ifdef __SIZEOF_INT128__
      || clrsbI (-1) != __SIZEOF_INT128__ * __CHAR_BIT__ - 1
#endif
      || clrsbi (0x1afb) != __SIZEOF_INT__ * __CHAR_BIT__ - 14
      || clrsbi (-2) != __SIZEOF_INT__ * __CHAR_BIT__ - 2
      || clrsbl (1L) != __SIZEOF_LONG__ * __CHAR_BIT__ - 2
      || clrsbL (-4LL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 3
      || ffsc (0) != 0
      || ffss (0) != 0
      || ffsi (0) != 0
      || ffsl (0L) != 0
      || ffsL (0LL) != 0
#ifdef __SIZEOF_INT128__
      || ffsI (0) != 0
#endif
      || ffsc (4) != 3
      || ffss (8) != 4
      || ffsi (1) != 1
      || ffsl (2L) != 2
      || ffsL (28LL) != 3
      || parityc (1) != 1
      || paritys (2) != 1
      || parityi (0U) != 0
      || parityi (3U) != 0
      || parityl (0UL) != 0
      || parityl (7UL) != 1
      || parityL (0ULL) != 0
#ifdef __SIZEOF_INT128__
      || parityI (0) != 0
#endif
      || parityc ((unsigned char) ~0U) != 0
      || paritys ((unsigned short) ~0U) != 0
      || parityi (~0U) != 0
      || parityl (~0UL) != 0
      || parityL (~0ULL) != 0
#ifdef __SIZEOF_INT128__
      || parityI (~(unsigned __int128) 0) != 0
#endif
      || popcounti (0U) != 0
      || popcountl (0UL) != 0
      || popcountL (0ULL) != 0
#ifdef __SIZEOF_INT128__
      || popcountI (0) != 0
#endif
      || popcountc ((unsigned char) ~0U) != __CHAR_BIT__
      || popcounts ((unsigned short) ~0U) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || popcounti (~0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || popcountl (~0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || popcountL (~0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__
#ifdef __SIZEOF_INT128__
      || popcountI (~(unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__
#endif
      || 0)
  __builtin_abort ();
}
