/* { dg-do run } */
/* { dg-options "-std=c11" } */

unsigned long long
rotate_left (unsigned char a, unsigned short b, unsigned int c,
	     unsigned long d, unsigned long long e, int n)
{
  return (__builtin_stdc_rotate_left (a, n)
	  + __builtin_stdc_rotate_left (b, n)
	  + __builtin_stdc_rotate_left (c, n)
	  + __builtin_stdc_rotate_left (d, n)
	  + __builtin_stdc_rotate_left (e, n));
}

unsigned long long
rotate_right (unsigned char a, unsigned short b, unsigned int c,
	      unsigned long d, unsigned long long e, int n)
{
  return (__builtin_stdc_rotate_right (a, n)
	  + __builtin_stdc_rotate_right (b, n)
	  + __builtin_stdc_rotate_right (c, n)
	  + __builtin_stdc_rotate_right (d, n)
	  + __builtin_stdc_rotate_right (e, n));
}

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

int
main ()
{
  if (__builtin_stdc_rotate_left ((unsigned char) 0, 0) != 0
      || __builtin_stdc_rotate_left ((unsigned char) 0xdcU, (char) 0) != 0xdcU
      || __builtin_stdc_rotate_left ((unsigned char) 0xdcU, __CHAR_BIT__) != 0xdcU
      || __builtin_stdc_rotate_left ((unsigned char) 0xdcU, 2 * __CHAR_BIT__) != 0xdcU
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned char) 0, 0), unsigned char)
      || __builtin_stdc_rotate_left ((unsigned char) 1, (_Bool) 1) != 2
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned char) 1, (unsigned char) 1), unsigned char)
      || __builtin_stdc_rotate_left ((unsigned char) ~1U, 3) != (unsigned char) ~8U
      || __builtin_stdc_rotate_left ((unsigned char) (2U + __SCHAR_MAX__), 1) != 3
      || __builtin_stdc_rotate_left ((unsigned char) (2U + __SCHAR_MAX__), __CHAR_BIT__ + 1) != 3
      || __builtin_stdc_rotate_left ((unsigned short) 0, 0) != 0
      || __builtin_stdc_rotate_left ((unsigned short) 0xdcabU, 0) != 0xdcabU
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned short) 0, (signed char) 0), unsigned short)
      || __builtin_stdc_rotate_left ((unsigned short) 1, 13) != (1U << 13)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned short) 1, 13), unsigned short)
      || __builtin_stdc_rotate_left ((unsigned short) ~1U, 7) != (unsigned short) ~0x80U
      || __builtin_stdc_rotate_left ((unsigned short) (2U + __SHRT_MAX__), 1) != 3
      || __builtin_stdc_rotate_left (0U, 0) != 0
      || __builtin_stdc_rotate_left (0xcdbaU, (short int) 0) != 0xcdbaU
      || !expr_has_type (__builtin_stdc_rotate_left (0U, 0), unsigned int)
      || __builtin_stdc_rotate_left (1U, (char) 15) != (1U << 15)
      || !expr_has_type (__builtin_stdc_rotate_left (1U, 15), unsigned int)
      || __builtin_stdc_rotate_left (~1U, 9) != ~0x200U
      || __builtin_stdc_rotate_left (2U + __INT_MAX__, 1) != 3
      || __builtin_stdc_rotate_left (0UL, 0) != 0
      || __builtin_stdc_rotate_left (0xdc8971baUL, 0) != 0xdc8971baUL
      || !expr_has_type (__builtin_stdc_rotate_left (0UL, 0LL), unsigned long)
      || __builtin_stdc_rotate_left (1UL, 30) != (1UL << 30)
      || !expr_has_type (__builtin_stdc_rotate_left (1UL, 30), unsigned long)
      || __builtin_stdc_rotate_left (~1UL, 22) != ~0x400000UL
      || __builtin_stdc_rotate_left (2UL + __LONG_MAX__, 1) != 3
      || __builtin_stdc_rotate_left (2UL + __LONG_MAX__, (int) (sizeof (unsigned long) * __CHAR_BIT__) + 1) != 3
      || __builtin_stdc_rotate_left (0ULL, (_Bool) 0) != 0
      || __builtin_stdc_rotate_left (0xdc897143985734baULL, 0) != 0xdc897143985734baULL
      || __builtin_stdc_rotate_left (0xdc897143985734baULL, 4 * (int) (sizeof (unsigned long long) * __CHAR_BIT__)) != 0xdc897143985734baULL
      || !expr_has_type (__builtin_stdc_rotate_left (0ULL, 0), unsigned long long)
      || __builtin_stdc_rotate_left (1ULL, 62) != (1ULL << 62)
      || !expr_has_type (__builtin_stdc_rotate_left (1ULL, 62ULL), unsigned long long)
      || __builtin_stdc_rotate_left (~1ULL, 53) != ~0x20000000000000ULL
      || __builtin_stdc_rotate_left (2ULL + __LONG_LONG_MAX__, 1) != 3)
    __builtin_abort ();
  if (__builtin_stdc_rotate_right ((unsigned char) 0, 0) != 0
      || __builtin_stdc_rotate_right ((unsigned char) 0xdcU, 0) != 0xdcU
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned char) 0, 0), unsigned char)
      || __builtin_stdc_rotate_right ((unsigned char) 1, 1) != (1U + __SCHAR_MAX__)
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned char) 1, 1), unsigned char)
      || __builtin_stdc_rotate_right ((unsigned char) ~1U, (char) 3) != (unsigned char) ~((1U + __SCHAR_MAX__) >> 2)
      || __builtin_stdc_rotate_right ((unsigned char) 3, 1) != (2U + __SCHAR_MAX__)
      || __builtin_stdc_rotate_right ((unsigned short) 0, 0) != 0
      || __builtin_stdc_rotate_right ((unsigned short) 0xdcabU, 0) != 0xdcabU
      || __builtin_stdc_rotate_right ((unsigned short) 0xdcabU, sizeof (unsigned short) * __CHAR_BIT__) != 0xdcabU
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned short) 0, 0), unsigned short)
      || __builtin_stdc_rotate_right ((unsigned short) (1U << 13), 12) != 2
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned short) (1U << 13), 2), unsigned short)
      || __builtin_stdc_rotate_right ((unsigned short) ~1U, 7) != (unsigned short) ~((1U + __SHRT_MAX__) >> 6)
      || __builtin_stdc_rotate_right ((unsigned short) 3, 1) != (2U + __SHRT_MAX__)
      || __builtin_stdc_rotate_right (0U, 0) != 0
      || __builtin_stdc_rotate_right (0xcdbaU, 0) != 0xcdbaU
      || !expr_has_type (__builtin_stdc_rotate_right (0U, 0), unsigned int)
      || __builtin_stdc_rotate_right (1U << 15, 13) != 4U
      || !expr_has_type (__builtin_stdc_rotate_right (1U << 15, 13), unsigned int)
      || __builtin_stdc_rotate_right (~1U, 9) != ~((1U + __INT_MAX__) >> 8)
      || __builtin_stdc_rotate_right (3U, (_Bool) 1) != (2U + __INT_MAX__)
      || __builtin_stdc_rotate_right (3U, (int) (sizeof (unsigned) * __CHAR_BIT__) + 1) != (2U + __INT_MAX__)
      || __builtin_stdc_rotate_right (0UL, (_Bool) 0) != 0
      || __builtin_stdc_rotate_right (0xdc8971baUL, 0) != 0xdc8971baUL
      || !expr_has_type (__builtin_stdc_rotate_right (0UL, 0), unsigned long)
      || __builtin_stdc_rotate_right (1UL << 30, 27) != 8UL
      || !expr_has_type (__builtin_stdc_rotate_right (1UL << 30, 27), unsigned long)
      || __builtin_stdc_rotate_right (~1UL, 22) != ~((1UL + __LONG_MAX__) >> 21)
      || __builtin_stdc_rotate_right (3UL, 1) != (2UL + __LONG_MAX__)
      || __builtin_stdc_rotate_right (0ULL, 0) != 0
      || __builtin_stdc_rotate_right (0xdc897143985734baULL, 0) != 0xdc897143985734baULL
      || !expr_has_type (__builtin_stdc_rotate_right (0ULL, 0), unsigned long long)
      || __builtin_stdc_rotate_right (1ULL << 62, 60) != 4ULL
      || !expr_has_type (__builtin_stdc_rotate_right (1ULL << 62, 60), unsigned long long)
      || __builtin_stdc_rotate_right (~1ULL, 53) != ~((1ULL + __LONG_LONG_MAX__) >> 52)
      || __builtin_stdc_rotate_right (3ULL, 1) != (2ULL + __LONG_LONG_MAX__))
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (__builtin_stdc_rotate_left ((unsigned __int128) 0U, 0) != 0
      || __builtin_stdc_rotate_left (((unsigned __int128) 0x43256567547ULL) << 64 | 0xdc897143985734baULL, 0) != (((unsigned __int128) 0x43256567547ULL) << 64 | 0xdc897143985734baULL)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned __int128) 0U, 0), unsigned __int128)
      || __builtin_stdc_rotate_left ((unsigned __int128) 1U, 115) != ((unsigned __int128) 1U << 115)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned __int128) 1U, 115), unsigned __int128)
      || __builtin_stdc_rotate_left (~(unsigned __int128) 1ULL, 53) != ~(unsigned __int128) 0x20000000000000ULL
      || __builtin_stdc_rotate_left (1ULL + ((unsigned __int128) 1U << 127), 1) != 3)
    __builtin_abort ();
  if (__builtin_stdc_rotate_right (0ULL, 0) != 0
      || __builtin_stdc_rotate_right (((unsigned __int128) 0x43256567547ULL) << 64 | 0xdc897143985734baULL, 0) != (((unsigned __int128) 0x43256567547ULL) << 64 | 0xdc897143985734baULL)
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned __int128) 0U, 0), unsigned __int128)
      || __builtin_stdc_rotate_right ((unsigned __int128) 1ULL << 115, 110) != 32U
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned __int128) 1ULL << 115, 110), unsigned __int128)
      || __builtin_stdc_rotate_right (~(unsigned __int128) 1ULL, 53) != ~((unsigned __int128) 1 << 75)
      || __builtin_stdc_rotate_right ((unsigned __int128) 3ULL, 1) != 1U + ((unsigned __int128) 1U << 127))
    __builtin_abort ();
#endif
#if __has_builtin (__builtin_stdc_rotate_left) != 1
#error __builtin_stdc_rotate_left not implemented
#endif
#if __has_builtin (__builtin_stdc_rotate_right) != 1
#error __builtin_stdc_rotate_right not implemented
#endif
  unsigned char a = 1;
  unsigned short b = 1;
  unsigned int c = 1;
  unsigned long d = 1;
  unsigned long long e = 1;
  int f = 1;
  if (__builtin_stdc_rotate_left (a++, f++) != 2 || a != 2 || f != 2)
    __builtin_abort ();
  if (__builtin_stdc_rotate_left (b++, f++) != 4 || b != 2 || f != 3)
    __builtin_abort ();
  if (__builtin_stdc_rotate_left (c++, f++) != 8 || c != 2 || f != 4)
    __builtin_abort ();
  if (__builtin_stdc_rotate_left (d++, f++) != 16 || d != 2 || f != 5)
    __builtin_abort ();
  if (__builtin_stdc_rotate_left (e++, f++) != 32 || e != 2 || f != 6)
    __builtin_abort ();
  f = 1;
  if (__builtin_stdc_rotate_right (a++, f++) != 1 || a != 3 || f != 2)
    __builtin_abort ();
  f = 1;
  if (__builtin_stdc_rotate_right (b++, f++) != 1 || b != 3 || f != 2)
    __builtin_abort ();
  f = 1;
  if (__builtin_stdc_rotate_right (c++, f++) != 1 || c != 3 || f != 2)
    __builtin_abort ();
  f = 1;
  if (__builtin_stdc_rotate_right (d++, f++) != 1 || d != 3 || f != 2)
    __builtin_abort ();
  f = 1;
  if (__builtin_stdc_rotate_right (e++, f++) != 1 || e != 3 || f != 2)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  unsigned __int128 g = 1;
  if (__builtin_stdc_rotate_left (g++, f++) != 4 || g != 2 || f != 3)
    __builtin_abort ();
  f = 1;
  if (__builtin_stdc_rotate_right (g++, f++) != 1 || g != 3 || f != 2)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 64
  if (__builtin_stdc_rotate_left (0uwb, 0) != 0
      || __builtin_stdc_rotate_left (1uwb, 0) != 1uwb
      || !expr_has_type (__builtin_stdc_rotate_left (0uwb, 0), unsigned _BitInt(1)))
    __builtin_abort ();
  if (__builtin_stdc_rotate_left ((unsigned _BitInt(2)) 0, 0) != 0
      || __builtin_stdc_rotate_left ((unsigned _BitInt(2)) 1, (_BitInt(27)) 1) != 2uwb
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned _BitInt(2)) 0, 0), unsigned _BitInt(2))
      || __builtin_stdc_rotate_left ((unsigned _BitInt(2)) 1, 1) != 2
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned _BitInt(2)) 1, (unsigned _BitInt(2)) 1), unsigned _BitInt(2))
      || __builtin_stdc_rotate_left ((unsigned _BitInt(2)) 2, 1) != 1)
    __builtin_abort ();
  if (__builtin_stdc_rotate_right (0uwb, 0) != 0
      || __builtin_stdc_rotate_right (1uwb, 0) != 1uwb
      || !expr_has_type (__builtin_stdc_rotate_right (0uwb, 0), unsigned _BitInt(1)))
    __builtin_abort ();
  if (__builtin_stdc_rotate_right ((unsigned _BitInt(2)) 0, (_BitInt(3)) 0) != 0
      || __builtin_stdc_rotate_right ((unsigned _BitInt(2)) 1, 1) != 2uwb
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned _BitInt(2)) 0, 0), unsigned _BitInt(2))
      || __builtin_stdc_rotate_right ((unsigned _BitInt(2)) 1, 1) != 2
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned _BitInt(2)) 1, 1), unsigned _BitInt(2))
      || __builtin_stdc_rotate_right ((unsigned _BitInt(2)) 2, 1) != 1)
    __builtin_abort ();
  if (__builtin_stdc_rotate_left ((unsigned _BitInt(59)) 0U, 0) != 0
      || __builtin_stdc_rotate_left ((unsigned _BitInt(59)) 0x43256567547ULL, 0) != ((unsigned _BitInt(59)) 0x43256567547ULL)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned _BitInt(59)) 0U, 0), unsigned _BitInt(59))
      || __builtin_stdc_rotate_left ((unsigned _BitInt(59)) 1U, 57) != ((unsigned _BitInt(59)) 1U << 57)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned _BitInt(59)) 1U, 57), unsigned _BitInt(59))
      || __builtin_stdc_rotate_left (~(unsigned _BitInt(59)) 1ULL, 53) != ~(unsigned _BitInt(59)) 0x20000000000000ULL
      || __builtin_stdc_rotate_left (1uwb + ((unsigned _BitInt(59)) 1U << 58), 1) != 3)
    __builtin_abort ();
  if (__builtin_stdc_rotate_right (0ULL, 0) != 0
      || __builtin_stdc_rotate_right ((unsigned _BitInt(59)) 0x43256567547ULL, 0) != ((unsigned _BitInt(59)) 0x43256567547ULL)
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned _BitInt(59)) 0U, 0), unsigned _BitInt(59))
      || __builtin_stdc_rotate_right ((unsigned _BitInt(59)) 1ULL << 57, 55) != 4U
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned _BitInt(59)) 1U << 57, 55), unsigned _BitInt(59))
      || __builtin_stdc_rotate_right (~(unsigned _BitInt(59)) 1ULL, 53) != ~((unsigned _BitInt(59)) 1 << 6)
      || __builtin_stdc_rotate_right ((unsigned _BitInt(59)) 3ULL, 1) != 1uwb + ((unsigned _BitInt(59)) 1U << 58))
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 512
  if (__builtin_stdc_rotate_left ((unsigned _BitInt(510)) 0U, (_BitInt(503)) 0) != 0
      || __builtin_stdc_rotate_left (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL, 0) != (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL)
      || __builtin_stdc_rotate_left (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL, 510) != (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned _BitInt(510)) 0U, 0), unsigned _BitInt(510))
      || __builtin_stdc_rotate_left ((unsigned _BitInt(510)) 1U, 508) != ((unsigned _BitInt(510)) 1U << 508)
      || !expr_has_type (__builtin_stdc_rotate_left ((unsigned _BitInt(510)) 1U, (unsigned _BitInt(505)) 508), unsigned _BitInt(510))
      || __builtin_stdc_rotate_left (~(unsigned _BitInt(510)) 1ULL, 53) != ~(unsigned _BitInt(510)) 0x20000000000000ULL
      || __builtin_stdc_rotate_left (1uwb + ((unsigned _BitInt(510)) 1U << 509), 1) != 3
      || __builtin_stdc_rotate_left (1uwb + ((unsigned _BitInt(510)) 1U << 509), 511U) != 3)
    __builtin_abort ();
  if (__builtin_stdc_rotate_right (0ULL, 0) != 0
      || __builtin_stdc_rotate_right (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL, 0) != (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL)
      || __builtin_stdc_rotate_right (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL, 510U * 2) != (((unsigned _BitInt(510)) 0x43256567547ULL) << 64 | 0xdc897143985734baULL)
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned _BitInt(510)) 0U, 0), unsigned _BitInt(510))
      || __builtin_stdc_rotate_right ((unsigned _BitInt(510)) 1ULL << 508, 503) != 32U
      || !expr_has_type (__builtin_stdc_rotate_right ((unsigned _BitInt(510)) 1ULL << 508, 503), unsigned _BitInt(510))
      || __builtin_stdc_rotate_right (~(unsigned _BitInt(510)) 1ULL, 499) != ~((unsigned _BitInt(510)) 1 << 11)
      || __builtin_stdc_rotate_right ((unsigned _BitInt(510)) 3ULL, 1) != 1uwb + ((unsigned _BitInt(510)) 1U << 509)
      || __builtin_stdc_rotate_right ((unsigned _BitInt(510)) 3ULL, 511) != 1uwb + ((unsigned _BitInt(510)) 1U << 509))
    __builtin_abort ();
#endif
}
