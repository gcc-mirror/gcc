/* { dg-do run } */
/* { dg-options "-std=c11" } */

unsigned int
leading_zeros (unsigned char a, unsigned short b, unsigned int c,
	       unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_leading_zeros (a)
	  + __builtin_stdc_leading_zeros (b)
	  + __builtin_stdc_leading_zeros (c)
	  + __builtin_stdc_leading_zeros (d)
	  + __builtin_stdc_leading_zeros (e));
}

unsigned int
leading_ones (unsigned char a, unsigned short b, unsigned int c,
	      unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_leading_ones (a)
	  + __builtin_stdc_leading_ones (b)
	  + __builtin_stdc_leading_ones (c)
	  + __builtin_stdc_leading_ones (d)
	  + __builtin_stdc_leading_ones (e));
}

unsigned int
trailing_zeros (unsigned char a, unsigned short b, unsigned int c,
		unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_trailing_zeros (a)
	  + __builtin_stdc_trailing_zeros (b)
	  + __builtin_stdc_trailing_zeros (c)
	  + __builtin_stdc_trailing_zeros (d)
	  + __builtin_stdc_trailing_zeros (e));
}

unsigned int
trailing_ones (unsigned char a, unsigned short b, unsigned int c,
	       unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_trailing_ones (a)
	  + __builtin_stdc_trailing_ones (b)
	  + __builtin_stdc_trailing_ones (c)
	  + __builtin_stdc_trailing_ones (d)
	  + __builtin_stdc_trailing_ones (e));
}

unsigned int
first_leading_zero (unsigned char a, unsigned short b, unsigned int c,
		    unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_first_leading_zero (a)
	  + __builtin_stdc_first_leading_zero (b)
	  + __builtin_stdc_first_leading_zero (c)
	  + __builtin_stdc_first_leading_zero (d)
	  + __builtin_stdc_first_leading_zero (e));
}

unsigned int
first_leading_one (unsigned char a, unsigned short b, unsigned int c,
		   unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_first_leading_one (a)
	  + __builtin_stdc_first_leading_one (b)
	  + __builtin_stdc_first_leading_one (c)
	  + __builtin_stdc_first_leading_one (d)
	  + __builtin_stdc_first_leading_one (e));
}

unsigned int
first_trailing_zero (unsigned char a, unsigned short b, unsigned int c,
		     unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_first_trailing_zero (a)
	  + __builtin_stdc_first_trailing_zero (b)
	  + __builtin_stdc_first_trailing_zero (c)
	  + __builtin_stdc_first_trailing_zero (d)
	  + __builtin_stdc_first_trailing_zero (e));
}

unsigned int
first_trailing_one (unsigned char a, unsigned short b, unsigned int c,
		    unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_first_trailing_one (a)
	  + __builtin_stdc_first_trailing_one (b)
	  + __builtin_stdc_first_trailing_one (c)
	  + __builtin_stdc_first_trailing_one (d)
	  + __builtin_stdc_first_trailing_one (e));
}

unsigned int
count_zeros (unsigned char a, unsigned short b, unsigned int c,
	     unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_count_zeros (a)
	  + __builtin_stdc_count_zeros (b)
	  + __builtin_stdc_count_zeros (c)
	  + __builtin_stdc_count_zeros (d)
	  + __builtin_stdc_count_zeros (e));
}

unsigned int
count_ones (unsigned char a, unsigned short b, unsigned int c,
	    unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_count_ones (a)
	  + __builtin_stdc_count_ones (b)
	  + __builtin_stdc_count_ones (c)
	  + __builtin_stdc_count_ones (d)
	  + __builtin_stdc_count_ones (e));
}

unsigned int
has_single_bit (unsigned char a, unsigned short b, unsigned int c,
		unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_has_single_bit (a)
	  || __builtin_stdc_has_single_bit (b)
	  || __builtin_stdc_has_single_bit (c)
	  || __builtin_stdc_has_single_bit (d)
	  || __builtin_stdc_has_single_bit (e));
}

unsigned int
bit_width (unsigned char a, unsigned short b, unsigned int c,
	   unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_bit_width (a)
	  + __builtin_stdc_bit_width (b)
	  + __builtin_stdc_bit_width (c)
	  + __builtin_stdc_bit_width (d)
	  + __builtin_stdc_bit_width (e));
}

unsigned long long
bit_floor (unsigned char a, unsigned short b, unsigned int c,
	   unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_bit_floor (a)
	  + __builtin_stdc_bit_floor (b)
	  + __builtin_stdc_bit_floor (c)
	  + __builtin_stdc_bit_floor (d)
	  + __builtin_stdc_bit_floor (e));
}

unsigned long long
bit_ceil (unsigned char a, unsigned short b, unsigned int c,
	  unsigned long d, unsigned long long e)
{
  return (__builtin_stdc_bit_ceil (a)
	  + __builtin_stdc_bit_ceil (b)
	  + __builtin_stdc_bit_ceil (c)
	  + __builtin_stdc_bit_ceil (d)
	  + __builtin_stdc_bit_ceil (e));
}

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

int
main ()
{
  if (__builtin_stdc_leading_zeros ((unsigned char) 0) != __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_leading_zeros ((unsigned char) 0), unsigned int)
      || __builtin_stdc_leading_zeros ((unsigned short) 0) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_leading_zeros ((unsigned short) 0), unsigned int)
      || __builtin_stdc_leading_zeros (0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_leading_zeros (0U), unsigned int)
      || __builtin_stdc_leading_zeros (0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_leading_zeros (0UL), unsigned int)
      || __builtin_stdc_leading_zeros (0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_leading_zeros (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_leading_zeros ((unsigned char) ~0U) != 0
      || __builtin_stdc_leading_zeros ((unsigned short) ~0U) != 0
      || __builtin_stdc_leading_zeros (~0U) != 0
      || __builtin_stdc_leading_zeros (~0UL) != 0
      || __builtin_stdc_leading_zeros (~0ULL) != 0)
    __builtin_abort ();
  if (__builtin_stdc_leading_zeros ((unsigned char) 3) != __CHAR_BIT__ - 2
      || __builtin_stdc_leading_zeros ((unsigned short) 9) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 4
      || __builtin_stdc_leading_zeros (34U) != __SIZEOF_INT__ * __CHAR_BIT__ - 6
      || __builtin_stdc_leading_zeros (130UL) != __SIZEOF_LONG__ * __CHAR_BIT__ - 8
      || __builtin_stdc_leading_zeros (512ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 10)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_leading_ones ((unsigned char) 0), unsigned int)
      || __builtin_stdc_leading_ones ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_leading_ones ((unsigned short) 0), unsigned int)
      || __builtin_stdc_leading_ones (0U) != 0
      || !expr_has_type (__builtin_stdc_leading_ones (0U), unsigned int)
      || __builtin_stdc_leading_ones (0UL) != 0
      || !expr_has_type (__builtin_stdc_leading_ones (0UL), unsigned int)
      || __builtin_stdc_leading_ones (0ULL) != 0
      || !expr_has_type (__builtin_stdc_leading_ones (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_leading_ones ((unsigned char) ~0U) != __CHAR_BIT__
      || __builtin_stdc_leading_ones ((unsigned short) ~0U) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || __builtin_stdc_leading_ones (~0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || __builtin_stdc_leading_ones (~0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || __builtin_stdc_leading_ones (~0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones ((unsigned char) ~3) != __CHAR_BIT__ - 2
      || __builtin_stdc_leading_ones ((unsigned short) ~9) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 4
      || __builtin_stdc_leading_ones (~34U) != __SIZEOF_INT__ * __CHAR_BIT__ - 6
      || __builtin_stdc_leading_ones (~130UL) != __SIZEOF_LONG__ * __CHAR_BIT__ - 8
      || __builtin_stdc_leading_ones (~512ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 10)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros ((unsigned char) 0) != __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_trailing_zeros ((unsigned char) 0), unsigned int)
      || __builtin_stdc_trailing_zeros ((unsigned short) 0) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_trailing_zeros ((unsigned short) 0), unsigned int)
      || __builtin_stdc_trailing_zeros (0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_trailing_zeros (0U), unsigned int)
      || __builtin_stdc_trailing_zeros (0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_trailing_zeros (0UL), unsigned int)
      || __builtin_stdc_trailing_zeros (0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_trailing_zeros (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros ((unsigned char) ~0U) != 0
      || __builtin_stdc_trailing_zeros ((unsigned short) ~0U) != 0
      || __builtin_stdc_trailing_zeros (~0U) != 0
      || __builtin_stdc_trailing_zeros (~0UL) != 0
      || __builtin_stdc_trailing_zeros (~0ULL) != 0)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros ((unsigned char) 2) != 1
      || __builtin_stdc_trailing_zeros ((unsigned short) 24) != 3
      || __builtin_stdc_trailing_zeros (32U) != 5
      || __builtin_stdc_trailing_zeros (128UL) != 7
      || __builtin_stdc_trailing_zeros (512ULL) != 9)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones ((unsigned char) 0), unsigned int)
      || __builtin_stdc_trailing_ones ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones ((unsigned short) 0), unsigned int)
      || __builtin_stdc_trailing_ones (0U) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones (0U), unsigned int)
      || __builtin_stdc_trailing_ones (0UL) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones (0UL), unsigned int)
      || __builtin_stdc_trailing_ones (0ULL) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones ((unsigned char) ~0U) != __CHAR_BIT__
      || __builtin_stdc_trailing_ones ((unsigned short) ~0U) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || __builtin_stdc_trailing_ones (~0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || __builtin_stdc_trailing_ones (~0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || __builtin_stdc_trailing_ones (~0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones ((unsigned char) 5) != 1
      || __builtin_stdc_trailing_ones ((unsigned short) 15) != 4
      || __builtin_stdc_trailing_ones (127U) != 7
      || __builtin_stdc_trailing_ones (511UL) != 9
      || __builtin_stdc_trailing_ones (~0ULL >> 2) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 2)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero ((unsigned char) 0) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero ((unsigned char) 0), unsigned int)
      || __builtin_stdc_first_leading_zero ((unsigned short) 0) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero ((unsigned short) 0), unsigned int)
      || __builtin_stdc_first_leading_zero (0U) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero (0U), unsigned int)
      || __builtin_stdc_first_leading_zero (0UL) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero (0UL), unsigned int)
      || __builtin_stdc_first_leading_zero (0ULL) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero ((unsigned char) ~0U) != 0
      || __builtin_stdc_first_leading_zero ((unsigned short) ~0U) != 0
      || __builtin_stdc_first_leading_zero (~0U) != 0
      || __builtin_stdc_first_leading_zero (~0UL) != 0
      || __builtin_stdc_first_leading_zero (~0ULL) != 0)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero ((unsigned char) ~3U) != __CHAR_BIT__ - 1
      || __builtin_stdc_first_leading_zero ((unsigned short) ~15U) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 3
      || __builtin_stdc_first_leading_zero (~63U) != __SIZEOF_INT__ * __CHAR_BIT__ - 5
      || __builtin_stdc_first_leading_zero (~255UL) != __SIZEOF_LONG__ * __CHAR_BIT__ - 7
      || __builtin_stdc_first_leading_zero (~1023ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 9)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one ((unsigned char) 0), unsigned int)
      || __builtin_stdc_first_leading_one ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one ((unsigned short) 0), unsigned int)
      || __builtin_stdc_first_leading_one (0U) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one (0U), unsigned int)
      || __builtin_stdc_first_leading_one (0UL) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one (0UL), unsigned int)
      || __builtin_stdc_first_leading_one (0ULL) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one ((unsigned char) ~0U) != 1
      || __builtin_stdc_first_leading_one ((unsigned short) ~0U) != 1
      || __builtin_stdc_first_leading_one (~0U) != 1
      || __builtin_stdc_first_leading_one (~0UL) != 1
      || __builtin_stdc_first_leading_one (~0ULL) != 1)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one ((unsigned char) 3) != __CHAR_BIT__ - 1
      || __builtin_stdc_first_leading_one ((unsigned short) 9) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 3
      || __builtin_stdc_first_leading_one (34U) != __SIZEOF_INT__ * __CHAR_BIT__ - 5
      || __builtin_stdc_first_leading_one (130UL) != __SIZEOF_LONG__ * __CHAR_BIT__ - 7
      || __builtin_stdc_first_leading_one (512ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 9)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero ((unsigned char) 0) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero ((unsigned char) 0), unsigned int)
      || __builtin_stdc_first_trailing_zero ((unsigned short) 0) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero ((unsigned short) 0), unsigned int)
      || __builtin_stdc_first_trailing_zero (0U) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero (0U), unsigned int)
      || __builtin_stdc_first_trailing_zero (0UL) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero (0UL), unsigned int)
      || __builtin_stdc_first_trailing_zero (0ULL) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero ((unsigned char) ~0U) != 0
      || __builtin_stdc_first_trailing_zero ((unsigned short) ~0U) != 0
      || __builtin_stdc_first_trailing_zero (~0U) != 0
      || __builtin_stdc_first_trailing_zero (~0UL) != 0
      || __builtin_stdc_first_trailing_zero (~0ULL) != 0)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero ((unsigned char) 2) != 1
      || __builtin_stdc_first_trailing_zero ((unsigned short) 15) != 5
      || __builtin_stdc_first_trailing_zero (63U) != 7
      || __builtin_stdc_first_trailing_zero (128UL) != 1
      || __builtin_stdc_first_trailing_zero (511ULL) != 10)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one ((unsigned char) 0), unsigned int)
      || __builtin_stdc_first_trailing_one ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one ((unsigned short) 0), unsigned int)
      || __builtin_stdc_first_trailing_one (0U) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one (0U), unsigned int)
      || __builtin_stdc_first_trailing_one (0UL) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one (0UL), unsigned int)
      || __builtin_stdc_first_trailing_one (0ULL) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one ((unsigned char) ~0U) != 1
      || __builtin_stdc_first_trailing_one ((unsigned short) ~0U) != 1
      || __builtin_stdc_first_trailing_one (~0U) != 1
      || __builtin_stdc_first_trailing_one (~0UL) != 1
      || __builtin_stdc_first_trailing_one (~0ULL) != 1)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one ((unsigned char) 4) != 3
      || __builtin_stdc_first_trailing_one ((unsigned short) 96) != 6
      || __builtin_stdc_first_trailing_one (127U) != 1
      || __builtin_stdc_first_trailing_one (511UL) != 1
      || __builtin_stdc_first_trailing_one (~0ULL << 12) != 13)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros ((unsigned char) 0) != __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_count_zeros ((unsigned char) 0), unsigned int)
      || __builtin_stdc_count_zeros ((unsigned short) 0) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_count_zeros ((unsigned short) 0), unsigned int)
      || __builtin_stdc_count_zeros (0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_count_zeros (0U), unsigned int)
      || __builtin_stdc_count_zeros (0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_count_zeros (0UL), unsigned int)
      || __builtin_stdc_count_zeros (0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_count_zeros (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_count_zeros ((unsigned char) ~0U) != 0
      || __builtin_stdc_count_zeros ((unsigned short) ~0U) != 0
      || __builtin_stdc_count_zeros (~0U) != 0
      || __builtin_stdc_count_zeros (~0UL) != 0
      || __builtin_stdc_count_zeros (~0ULL) != 0)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros ((unsigned char) 1U) != __CHAR_BIT__ - 1
      || __builtin_stdc_count_zeros ((unsigned short) 42) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 3
      || __builtin_stdc_count_zeros (291U) != __SIZEOF_INT__ * __CHAR_BIT__ - 4
      || __builtin_stdc_count_zeros (~1315UL) != 5
      || __builtin_stdc_count_zeros (3363ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 6)
    __builtin_abort ();
  if (__builtin_stdc_count_ones ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_count_ones ((unsigned char) 0), unsigned int)
      || __builtin_stdc_count_ones ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_count_ones ((unsigned short) 0), unsigned int)
      || __builtin_stdc_count_ones (0U) != 0
      || !expr_has_type (__builtin_stdc_count_ones (0U), unsigned int)
      || __builtin_stdc_count_ones (0UL) != 0
      || !expr_has_type (__builtin_stdc_count_ones (0UL), unsigned int)
      || __builtin_stdc_count_ones (0ULL) != 0
      || !expr_has_type (__builtin_stdc_count_ones (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_count_ones ((unsigned char) ~0U) != __CHAR_BIT__
      || __builtin_stdc_count_ones ((unsigned short) ~0U) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || __builtin_stdc_count_ones (~0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || __builtin_stdc_count_ones (~0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || __builtin_stdc_count_ones (~0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_count_ones ((unsigned char) ~1U) != __CHAR_BIT__ - 1
      || __builtin_stdc_count_ones ((unsigned short) ~42) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 3
      || __builtin_stdc_count_ones (~291U) != __SIZEOF_INT__ * __CHAR_BIT__ - 4
      || __builtin_stdc_count_ones (1315UL) != 5
      || __builtin_stdc_count_ones (~3363ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 6)
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit ((unsigned char) 0)
      || !expr_has_type (__builtin_stdc_has_single_bit ((unsigned char) 0), _Bool)
      || __builtin_stdc_has_single_bit ((unsigned short) 0)
      || !expr_has_type (__builtin_stdc_has_single_bit ((unsigned short) 0), _Bool)
      || __builtin_stdc_has_single_bit (0U)
      || !expr_has_type (__builtin_stdc_has_single_bit (0U), _Bool)
      || __builtin_stdc_has_single_bit (0UL)
      || !expr_has_type (__builtin_stdc_has_single_bit (0UL), _Bool)
      || __builtin_stdc_has_single_bit (0ULL)
      || !expr_has_type (__builtin_stdc_has_single_bit (0ULL), _Bool))
    __builtin_abort ();
  if (!__builtin_stdc_has_single_bit ((unsigned char) 2)
      || !__builtin_stdc_has_single_bit ((unsigned short) 8)
      || !__builtin_stdc_has_single_bit (32U)
      || !__builtin_stdc_has_single_bit (128UL)
      || !__builtin_stdc_has_single_bit (512ULL))
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit ((unsigned char) 7)
      || __builtin_stdc_has_single_bit ((unsigned short) 96)
      || __builtin_stdc_has_single_bit (513U)
      || __builtin_stdc_has_single_bit (1022UL)
      || __builtin_stdc_has_single_bit (12ULL))
    __builtin_abort ();
  if (__builtin_stdc_bit_width ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_width ((unsigned char) 0), unsigned int)
      || __builtin_stdc_bit_width ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_width ((unsigned short) 0), unsigned int)
      || __builtin_stdc_bit_width (0U) != 0
      || !expr_has_type (__builtin_stdc_bit_width (0U), unsigned int)
      || __builtin_stdc_bit_width (0UL) != 0
      || !expr_has_type (__builtin_stdc_bit_width (0UL), unsigned int)
      || __builtin_stdc_bit_width (0ULL) != 0
      || !expr_has_type (__builtin_stdc_bit_width (0ULL), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_bit_width ((unsigned char) ~0U) != __CHAR_BIT__
      || __builtin_stdc_bit_width ((unsigned short) ~0U) != __SIZEOF_SHORT__ * __CHAR_BIT__
      || __builtin_stdc_bit_width (~0U) != __SIZEOF_INT__ * __CHAR_BIT__
      || __builtin_stdc_bit_width (~0UL) != __SIZEOF_LONG__ * __CHAR_BIT__
      || __builtin_stdc_bit_width (~0ULL) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_bit_width ((unsigned char) ((unsigned char) ~0U >> 1)) != __CHAR_BIT__ - 1
      || __builtin_stdc_bit_width ((unsigned char) 6) != 3
      || __builtin_stdc_bit_width ((unsigned short) 12U) != 4
      || __builtin_stdc_bit_width ((unsigned short) ((unsigned short) ~0U >> 5)) != __SIZEOF_SHORT__ * __CHAR_BIT__ - 5
      || __builtin_stdc_bit_width (137U) != 8
      || __builtin_stdc_bit_width (269U) != 9
      || __builtin_stdc_bit_width (39UL) != 6
      || __builtin_stdc_bit_width (~0UL >> 2) != __SIZEOF_LONG__ * __CHAR_BIT__ - 2
      || __builtin_stdc_bit_width (1023ULL) != 10
      || __builtin_stdc_bit_width (1024ULL) != 11)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor ((unsigned char) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_floor ((unsigned char) 0), unsigned char)
      || __builtin_stdc_bit_floor ((unsigned short) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_floor ((unsigned short) 0), unsigned short)
      || __builtin_stdc_bit_floor (0U) != 0U
      || !expr_has_type (__builtin_stdc_bit_floor (0U), unsigned int)
      || __builtin_stdc_bit_floor (0UL) != 0UL
      || !expr_has_type (__builtin_stdc_bit_floor (0UL), unsigned long)
      || __builtin_stdc_bit_floor (0ULL) != 0ULL
      || !expr_has_type (__builtin_stdc_bit_floor (0ULL), unsigned long long))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor ((unsigned char) ~0U) != (1U << (__CHAR_BIT__ - 1))
      || __builtin_stdc_bit_floor ((unsigned short) ~0U) != (1U << (__SIZEOF_SHORT__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_floor (~0U) != (1U << (__SIZEOF_INT__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_floor (~0UL) != (1UL << (__SIZEOF_LONG__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_floor (~0ULL) != (1ULL << (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1)))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor ((unsigned char) 4) != 4
      || __builtin_stdc_bit_floor ((unsigned char) 7) != 4
      || __builtin_stdc_bit_floor ((unsigned short) 8U) != 8
      || __builtin_stdc_bit_floor ((unsigned short) 31U) != 16
      || __builtin_stdc_bit_floor (137U) != 128U
      || __builtin_stdc_bit_floor (269U) != 256U
      || __builtin_stdc_bit_floor (511UL) != 256UL
      || __builtin_stdc_bit_floor (512UL) != 512UL
      || __builtin_stdc_bit_floor (513UL) != 512ULL
      || __builtin_stdc_bit_floor (1024ULL) != 1024ULL)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned char) 0) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil ((unsigned char) 0), unsigned char)
      || __builtin_stdc_bit_ceil ((unsigned short) 0) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil ((unsigned short) 0), unsigned short)
      || __builtin_stdc_bit_ceil (0U) != 1U
      || !expr_has_type (__builtin_stdc_bit_ceil (0U), unsigned int)
      || __builtin_stdc_bit_ceil (0UL) != 1UL
      || !expr_has_type (__builtin_stdc_bit_ceil (0UL), unsigned long)
      || __builtin_stdc_bit_ceil (0ULL) != 1ULL
      || !expr_has_type (__builtin_stdc_bit_ceil (0ULL), unsigned long long))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned char) ~0U) != 0
      || __builtin_stdc_bit_ceil ((unsigned short) ~0U) != 0
      || __builtin_stdc_bit_ceil (~0U) != 0U
      || __builtin_stdc_bit_ceil (~0UL) != 0UL
      || __builtin_stdc_bit_ceil (~0ULL) != 0ULL)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned char) ((unsigned char) ~0U >> 1)) != (1U << (__CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil ((unsigned char) ((unsigned char) ~0U >> 1)) != (1U << (__CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil ((unsigned short) ((unsigned short) ~0U >> 1)) != (1U << (__SIZEOF_SHORT__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil ((unsigned short) ((unsigned short) ~0U >> 1)) != (1U << (__SIZEOF_SHORT__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil (~0U >> 1) != (1U << (__SIZEOF_INT__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil (1U << (__SIZEOF_INT__ * __CHAR_BIT__ - 1)) != (1U << (__SIZEOF_INT__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil (~0UL >> 1) != (1UL << (__SIZEOF_LONG__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil (~0UL >> 1) != (1UL << (__SIZEOF_LONG__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil (1ULL << (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1)) != (1ULL << (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1))
      || __builtin_stdc_bit_ceil (~0ULL >> 1) != (1ULL << (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1)))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned char) 1) != 1
      || __builtin_stdc_bit_ceil ((unsigned char) 2) != 2
      || __builtin_stdc_bit_ceil ((unsigned short) 3U) != 4
      || __builtin_stdc_bit_ceil ((unsigned short) 4U) != 4
      || __builtin_stdc_bit_ceil (5U) != 8U
      || __builtin_stdc_bit_ceil (269U) != 512U
      || __builtin_stdc_bit_ceil (511UL) != 512UL
      || __builtin_stdc_bit_ceil (512UL) != 512UL
      || __builtin_stdc_bit_ceil (513ULL) != 1024ULL
      || __builtin_stdc_bit_ceil (1025ULL) != 2048ULL)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (__builtin_stdc_leading_zeros ((unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_leading_zeros ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_leading_zeros (~(unsigned __int128) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_leading_ones ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_leading_ones (~(unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros ((unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_trailing_zeros ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_trailing_zeros (~(unsigned __int128) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_trailing_ones (~(unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero ((unsigned __int128) 0) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_first_leading_zero (~(unsigned __int128) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_first_leading_one (~(unsigned __int128) 0) != 1)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero ((unsigned __int128) 0) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_first_trailing_zero (~(unsigned __int128) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_first_trailing_one (~(unsigned __int128) 0) != 1)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros ((unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__
      || !expr_has_type (__builtin_stdc_count_zeros ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_count_zeros (~(unsigned __int128) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_count_ones ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_count_ones ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_count_ones (~(unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit ((unsigned __int128) 0)
      || !expr_has_type (__builtin_stdc_has_single_bit ((unsigned __int128) 0), _Bool)
      || __builtin_stdc_has_single_bit (~(unsigned __int128) 0))
    __builtin_abort ();
  if (__builtin_stdc_bit_width ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_width ((unsigned __int128) 0), unsigned int)
      || __builtin_stdc_bit_width (~(unsigned __int128) 0) != __SIZEOF_INT128__ * __CHAR_BIT__)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor ((unsigned __int128) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_floor ((unsigned __int128) 0), unsigned __int128)
      || __builtin_stdc_bit_floor (~(unsigned __int128) 0) != ((unsigned __int128) 1) << (__SIZEOF_INT128__ * __CHAR_BIT__ - 1))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned __int128) 0) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil ((unsigned __int128) 0), unsigned __int128)
      || __builtin_stdc_bit_ceil ((unsigned __int128) 1) != 1
      || __builtin_stdc_bit_ceil ((~(unsigned __int128) 0) >> 1) != ((unsigned __int128) 1) << (__SIZEOF_INT128__ * __CHAR_BIT__ - 1)
      || __builtin_stdc_bit_ceil (~(unsigned __int128) 0) != 0)
    __builtin_abort ();
#endif
#if __has_builtin (__builtin_stdc_leading_zeros) != 1
#error __builtin_stdc_leading_zeros not implemented
#endif
#if __has_builtin (__builtin_stdc_leading_ones) != 1
#error __builtin_stdc_leading_ones not implemented
#endif
#if __has_builtin (__builtin_stdc_trailing_zeros) != 1
#error __builtin_stdc_trailing_zeros not implemented
#endif
#if __has_builtin (__builtin_stdc_trailing_ones) != 1
#error __builtin_stdc_trailing_ones not implemented
#endif
#if __has_builtin (__builtin_stdc_first_leading_zero) != 1
#error __builtin_stdc_first_leading_zero not implemented
#endif
#if __has_builtin (__builtin_stdc_first_leading_one) != 1
#error __builtin_stdc_first_leading_one not implemented
#endif
#if __has_builtin (__builtin_stdc_first_trailing_zero) != 1
#error __builtin_stdc_first_trailing_zero not implemented
#endif
#if __has_builtin (__builtin_stdc_first_trailing_one) != 1
#error __builtin_stdc_first_trailing_one not implemented
#endif
#if __has_builtin (__builtin_stdc_count_zeros) != 1
#error __builtin_stdc_count_zeros not implemented
#endif
#if __has_builtin (__builtin_stdc_count_ones) != 1
#error __builtin_stdc_count_ones not implemented
#endif
#if __has_builtin (__builtin_stdc_has_single_bit) != 1
#error __builtin_stdc_single_bit not implemented
#endif
#if __has_builtin (__builtin_stdc_bit_width) != 1
#error __builtin_stdc_bit_width not implemented
#endif
#if __has_builtin (__builtin_stdc_bit_floor) != 1
#error __builtin_stdc_bit_floor not implemented
#endif
#if __has_builtin (__builtin_stdc_bit_ceil) != 1
#error __builtin_stdc_bit_ceil not implemented
#endif
  unsigned char a = 0;
  if (__builtin_stdc_bit_width (a++) != 0 || a != 1)
    __builtin_abort ();
  unsigned long long b = 0;
  if (__builtin_stdc_bit_width (b++) != 0 || b != 1)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (a++) != 1 || a != 2)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (b++) != 1 || b != 2)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (a++) != 2 || a != 3)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (b++) != 2 || b != 3)
    __builtin_abort ();
  if (__builtin_stdc_leading_zeros (a++) != __CHAR_BIT__ - 2 || a != 4)
    __builtin_abort ();
  if (__builtin_stdc_leading_zeros (b++) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 2 || b != 4)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones (a++) != 0 || a != 5)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones (b++) != 0 || b != 5)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros (a++) != 0 || a != 6)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros (b++) != 0 || b != 6)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones (a++) != 0 || a != 7)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones (b++) != 0 || b != 7)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero (a++) != 1 || a != 8)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero (b++) != 1 || b != 8)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one (a++) != __CHAR_BIT__ - 3 || a != 9)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one (b++) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 3 || b != 9)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero (a++) != 2 || a != 10)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero (b++) != 2 || b != 10)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one (a++) != 2 || a != 11)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one (b++) != 2 || b != 11)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros (a++) != __CHAR_BIT__ - 3 || a != 12)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros (b++) != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 3 || b != 12)
    __builtin_abort ();
  if (__builtin_stdc_count_ones (a++) != 2 || a != 13)
    __builtin_abort ();
  if (__builtin_stdc_count_ones (b++) != 2 || b != 13)
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit (a++) || a != 14)
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit (b++) || b != 14)
    __builtin_abort ();
#if __BITINT_MAXWIDTH__ >= 64
  if (__builtin_stdc_leading_zeros (0uwb) != 1
      || !expr_has_type (__builtin_stdc_leading_zeros (0uwb), unsigned int)
      || __builtin_stdc_leading_zeros (1uwb) != 0
      || !expr_has_type (__builtin_stdc_leading_zeros (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_leading_ones (0uwb) != 0
      || !expr_has_type (__builtin_stdc_leading_ones (0uwb), unsigned int)
      || __builtin_stdc_leading_ones (1uwb) != 1
      || !expr_has_type (__builtin_stdc_leading_ones (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros (0uwb) != 1
      || !expr_has_type (__builtin_stdc_trailing_zeros (0uwb), unsigned int)
      || __builtin_stdc_trailing_zeros (1uwb) != 0
      || !expr_has_type (__builtin_stdc_trailing_zeros (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones (0uwb) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones (0uwb), unsigned int)
      || __builtin_stdc_trailing_ones (1uwb) != 1
      || !expr_has_type (__builtin_stdc_trailing_ones (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero (0uwb) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero (0uwb), unsigned int)
      || __builtin_stdc_first_leading_zero (1uwb) != 0
      || !expr_has_type (__builtin_stdc_first_leading_zero (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one (0uwb) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one (0uwb), unsigned int)
      || __builtin_stdc_first_leading_one (1uwb) != 1
      || !expr_has_type (__builtin_stdc_first_leading_one (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero (0uwb) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero (0uwb), unsigned int)
      || __builtin_stdc_first_trailing_zero (1uwb) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_zero (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one (0uwb) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one (0uwb), unsigned int)
      || __builtin_stdc_first_trailing_one (1uwb) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_one (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_count_zeros (0uwb) != 1
      || !expr_has_type (__builtin_stdc_count_zeros (0uwb), unsigned int)
      || __builtin_stdc_count_zeros (1uwb) != 0
      || !expr_has_type (__builtin_stdc_count_zeros (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_count_ones (0uwb) != 0
      || !expr_has_type (__builtin_stdc_count_ones (0uwb), unsigned int)
      || __builtin_stdc_count_ones (1uwb) != 1
      || !expr_has_type (__builtin_stdc_count_ones (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit (0uwb)
      || !expr_has_type (__builtin_stdc_has_single_bit (0uwb), _Bool)
      || !__builtin_stdc_has_single_bit (1uwb)
      || !expr_has_type (__builtin_stdc_has_single_bit (1uwb), _Bool))
    __builtin_abort ();
  if (__builtin_stdc_bit_width (0uwb) != 0
      || !expr_has_type (__builtin_stdc_bit_width (0uwb), unsigned int)
      || __builtin_stdc_bit_width (1uwb) != 1
      || !expr_has_type (__builtin_stdc_bit_width (1uwb), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (0uwb) != 0
      || !expr_has_type (__builtin_stdc_bit_floor (0uwb), unsigned _BitInt(1))
      || __builtin_stdc_bit_floor (1uwb) != 1
      || !expr_has_type (__builtin_stdc_bit_floor (1uwb), unsigned _BitInt(1)))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (0uwb) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil (0uwb), unsigned _BitInt(1))
      || __builtin_stdc_bit_ceil (1uwb) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil (1uwb), unsigned _BitInt(1)))
    __builtin_abort ();
  unsigned _BitInt(1) c = 0;
  if (__builtin_stdc_bit_floor (c++) != 0 || c != 1)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (c++) != 1 || c != 0)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (c++) != 1 || c != 1)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (c++) != 1 || c != 0)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 512
  if (__builtin_stdc_leading_zeros ((unsigned _BitInt(512)) 0) != 512
      || !expr_has_type (__builtin_stdc_leading_zeros ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_leading_zeros ((unsigned _BitInt(373)) 0) != 373
      || !expr_has_type (__builtin_stdc_leading_zeros ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_leading_zeros (~(unsigned _BitInt(512)) 0) != 0
      || __builtin_stdc_leading_zeros (~(unsigned _BitInt(373)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_leading_zeros ((unsigned _BitInt(512)) 275) != 512 - 9
      || __builtin_stdc_leading_zeros ((unsigned _BitInt(373)) 512) != 373 - 10)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_leading_ones ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_leading_ones ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_leading_ones ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_leading_ones (~(unsigned _BitInt(512)) 0) != 512
      || __builtin_stdc_leading_ones (~(unsigned _BitInt(373)) 0) != 373)
    __builtin_abort ();
  if (__builtin_stdc_leading_ones (~(unsigned _BitInt(512)) 275) != 512 - 9
      || __builtin_stdc_leading_ones (~(unsigned _BitInt(373)) 512) != 373 - 10)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros ((unsigned _BitInt(512)) 0) != 512
      || !expr_has_type (__builtin_stdc_trailing_zeros ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_trailing_zeros ((unsigned _BitInt(373)) 0) != 373
      || !expr_has_type (__builtin_stdc_trailing_zeros ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros (~(unsigned _BitInt(512)) 0) != 0
      || __builtin_stdc_trailing_zeros (~(unsigned _BitInt(373)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_trailing_zeros ((unsigned _BitInt(512)) 256) != 8
      || __builtin_stdc_trailing_zeros ((unsigned _BitInt(373)) 512) != 9)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_trailing_ones ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_trailing_ones ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones (~(unsigned _BitInt(512)) 0) != 512
      || __builtin_stdc_trailing_ones (~(unsigned _BitInt(373)) 0) != 373)
    __builtin_abort ();
  if (__builtin_stdc_trailing_ones ((unsigned _BitInt(512)) 255) != 8
      || __builtin_stdc_trailing_ones ((~(unsigned _BitInt(373)) 0) >> 2) != 373 - 2)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero ((unsigned _BitInt(512)) 0) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_first_leading_zero ((unsigned _BitInt(373)) 0) != 1
      || !expr_has_type (__builtin_stdc_first_leading_zero ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero (~(unsigned _BitInt(512)) 0) != 0
      || __builtin_stdc_first_leading_zero (~(unsigned _BitInt(373)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_zero (~(unsigned _BitInt(512)) 511) != 512 - 8
      || __builtin_stdc_first_leading_zero (~(unsigned _BitInt(373)) 1023) != 373 - 9)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_first_leading_one ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_first_leading_one ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one (~(unsigned _BitInt(512)) 0) != 1
      || __builtin_stdc_first_leading_one (~(unsigned _BitInt(373)) 0) != 1)
    __builtin_abort ();
  if (__builtin_stdc_first_leading_one ((unsigned _BitInt(512)) 275) != 512 - 8
      || __builtin_stdc_first_leading_one ((unsigned _BitInt(373)) 512) != 373 - 9)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero ((unsigned _BitInt(512)) 0) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_first_trailing_zero ((unsigned _BitInt(373)) 0) != 1
      || !expr_has_type (__builtin_stdc_first_trailing_zero ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero (~(unsigned _BitInt(512)) 0) != 0
      || __builtin_stdc_first_trailing_zero (~(unsigned _BitInt(373)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_zero ((unsigned _BitInt(512)) 255) != 9
      || __builtin_stdc_first_trailing_zero ((unsigned _BitInt(373)) 511) != 10)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_first_trailing_one ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_first_trailing_one ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one (~(unsigned _BitInt(512)) 0) != 1
      || __builtin_stdc_first_trailing_one (~(unsigned _BitInt(373)) 0) != 1)
    __builtin_abort ();
  if (__builtin_stdc_first_trailing_one (((unsigned _BitInt(512)) 255) << 175) != 176
      || __builtin_stdc_first_trailing_one ((~(unsigned _BitInt(373)) 0) << 311) != 312)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros ((unsigned _BitInt(512)) 0) != 512
      || !expr_has_type (__builtin_stdc_count_zeros ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_count_zeros ((unsigned _BitInt(373)) 0) != 373
      || !expr_has_type (__builtin_stdc_count_zeros ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_count_zeros (~(unsigned _BitInt(512)) 0) != 0
      || __builtin_stdc_count_zeros (~(unsigned _BitInt(373)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_count_zeros ((unsigned _BitInt(512)) 1315) != 512 - 5
      || __builtin_stdc_count_zeros ((unsigned _BitInt(373)) 3363) != 373 - 6)
    __builtin_abort ();
  if (__builtin_stdc_count_ones ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_count_ones ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_count_ones ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_count_ones ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_count_ones (~(unsigned _BitInt(512)) 0) != 512
      || __builtin_stdc_count_ones (~(unsigned _BitInt(373)) 0) != 373)
    __builtin_abort ();
  if (__builtin_stdc_count_ones (~(unsigned _BitInt(512)) 1315) != 512 - 5
      || __builtin_stdc_count_ones (~(unsigned _BitInt(373)) 3363) != 373 - 6)
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit ((unsigned _BitInt(512)) 0)
      || !expr_has_type (__builtin_stdc_has_single_bit ((unsigned _BitInt(512)) 0), _Bool)
      || __builtin_stdc_has_single_bit ((unsigned _BitInt(373)) 0)
      || !expr_has_type (__builtin_stdc_has_single_bit ((unsigned _BitInt(373)) 0), _Bool))
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit (~(unsigned _BitInt(512)) 0)
      || __builtin_stdc_has_single_bit (~(unsigned _BitInt(373)) 0))
    __builtin_abort ();
  if (__builtin_stdc_has_single_bit (((unsigned _BitInt(512)) 1022) << 279)
      || __builtin_stdc_has_single_bit (((unsigned _BitInt(373)) 12) << 305))
    __builtin_abort ();
  if (__builtin_stdc_bit_width ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_width ((unsigned _BitInt(512)) 0), unsigned int)
      || __builtin_stdc_bit_width ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_width ((unsigned _BitInt(373)) 0), unsigned int))
    __builtin_abort ();
  if (__builtin_stdc_bit_width (~(unsigned _BitInt(512)) 0) != 512
      || __builtin_stdc_bit_width (~(unsigned _BitInt(373)) 0) != 373)
    __builtin_abort ();
  if (__builtin_stdc_bit_width (((unsigned _BitInt(512)) 1023) << 405) != 405 + 10
      || __builtin_stdc_bit_width (((unsigned _BitInt(373)) 1024) << 242) != 242 + 11)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor ((unsigned _BitInt(512)) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_floor ((unsigned _BitInt(512)) 0), unsigned _BitInt(512))
      || __builtin_stdc_bit_floor ((unsigned _BitInt(373)) 0) != 0
      || !expr_has_type (__builtin_stdc_bit_floor ((unsigned _BitInt(373)) 0), unsigned _BitInt(373)))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (~(unsigned _BitInt(512)) 0) != ((unsigned _BitInt(512)) 1) << (512 - 1)
      || __builtin_stdc_bit_floor (~(unsigned _BitInt(373)) 0) != ((unsigned _BitInt(373)) 1) << (373 - 1))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (((unsigned _BitInt(512)) 511) << 405) != (((unsigned _BitInt(512)) 256) << 405)
      || __builtin_stdc_bit_floor (((unsigned _BitInt(373)) 512) << 242) != (((unsigned _BitInt(512)) 512) << 242))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned _BitInt(512)) 0) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil ((unsigned _BitInt(512)) 0), unsigned _BitInt(512))
      || __builtin_stdc_bit_ceil ((unsigned _BitInt(373)) 0) != 1
      || !expr_has_type (__builtin_stdc_bit_ceil ((unsigned _BitInt(373)) 0), unsigned _BitInt(373)))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (~(unsigned _BitInt(512)) 0) != 0
      || __builtin_stdc_bit_ceil (~(unsigned _BitInt(373)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (((unsigned _BitInt(512)) 1) << (512 - 1)) != ((unsigned _BitInt(512)) 1) << (512 - 1)
      || __builtin_stdc_bit_ceil ((~(unsigned _BitInt(373)) 0) >> 1) != ((unsigned _BitInt(373)) 1) << (373 - 1))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (((unsigned _BitInt(512)) 512) << 405) != (((unsigned _BitInt(512)) 512) << 405)
      || __builtin_stdc_bit_ceil (((unsigned _BitInt(373)) 513) << 242) != (((unsigned _BitInt(512)) 1024) << 242))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor ((unsigned _BitInt(__BITINT_MAXWIDTH__)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (~(unsigned _BitInt(__BITINT_MAXWIDTH__)) 0) != ((unsigned _BitInt(__BITINT_MAXWIDTH__)) 1) << (__BITINT_MAXWIDTH__ - 1))
    __builtin_abort ();
  if (__builtin_stdc_bit_floor (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 511) << 405) != (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 256) << 405)
      || __builtin_stdc_bit_floor (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 512) << 405) != (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 512) << 405))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil ((unsigned _BitInt(__BITINT_MAXWIDTH__)) 0) != 1)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (~(unsigned _BitInt(__BITINT_MAXWIDTH__)) 0) != 0)
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 1) << (__BITINT_MAXWIDTH__ - 1)) != ((unsigned _BitInt(__BITINT_MAXWIDTH__)) 1) << (__BITINT_MAXWIDTH__ - 1))
    __builtin_abort ();
  if (__builtin_stdc_bit_ceil (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 512) << 405) != (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 512) << 405)
      || __builtin_stdc_bit_ceil (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 513) << 405) != (((unsigned _BitInt(__BITINT_MAXWIDTH__)) 1024) << 405))
    __builtin_abort ();
#endif
}
