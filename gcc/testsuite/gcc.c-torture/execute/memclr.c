/* { dg-require-effective-target run_expensive_tests } */
/* { dg-timeout-factor 4 } */
/* { dg-skip-if "memory full + time hog" { "avr-*-*" } } */

typedef unsigned int __attribute__ ((mode (QI))) int08_t;
typedef unsigned int __attribute__ ((mode (HI))) int16_t;
typedef unsigned int __attribute__ ((mode (SI))) int32_t;
typedef unsigned int __attribute__ ((mode (DI))) int64_t;

typedef union
  {
    int08_t v[88];
  }
a1_t;

typedef union
  {
    int08_t v[88];
    int16_t a;
  }
a2_t;

typedef union
  {
    int08_t v[88];
    int32_t a;
  }
a4_t;

typedef union
  {
    int08_t v[88];
    int64_t a;
  }
a8_t;

#define MEMCLR_DEFINE_ONE(align, offset, count)				\
  static void __attribute__ ((noinline))				\
  memclr_check_one_ ## align ## offset ## count (void)			\
    {									\
      static a ## align ## _t dst = {{ [0 ... 87] = 0xaa }};		\
      int i;								\
									\
      __builtin_memset (dst.v + 8 + offset, 0, count);			\
      asm ("" : : : "memory");						\
      for (i = 0; i < 8 + offset; i++)					\
	if (dst.v[i] != 0xaa)						\
	  __builtin_abort ();						\
      for (; i < 8 + offset + count; i++)				\
	if (dst.v[i] != 0x00)						\
	  __builtin_abort ();						\
      for (; i < sizeof (dst.v); i++)					\
	if (dst.v[i] != 0xaa)						\
	  __builtin_abort ();						\
    }

#define MEMCLR_DEFINE_ONE_ALIGN_OFFSET(align, offset)			\
  MEMCLR_DEFINE_ONE (align, offset,  1)					\
  MEMCLR_DEFINE_ONE (align, offset,  2)					\
  MEMCLR_DEFINE_ONE (align, offset,  3)					\
  MEMCLR_DEFINE_ONE (align, offset,  4)					\
  MEMCLR_DEFINE_ONE (align, offset,  5)					\
  MEMCLR_DEFINE_ONE (align, offset,  6)					\
  MEMCLR_DEFINE_ONE (align, offset,  7)					\
  MEMCLR_DEFINE_ONE (align, offset,  8)					\
  MEMCLR_DEFINE_ONE (align, offset,  9)					\
  MEMCLR_DEFINE_ONE (align, offset, 10)					\
  MEMCLR_DEFINE_ONE (align, offset, 11)					\
  MEMCLR_DEFINE_ONE (align, offset, 12)					\
  MEMCLR_DEFINE_ONE (align, offset, 13)					\
  MEMCLR_DEFINE_ONE (align, offset, 14)					\
  MEMCLR_DEFINE_ONE (align, offset, 15)					\
  MEMCLR_DEFINE_ONE (align, offset, 16)					\
  MEMCLR_DEFINE_ONE (align, offset, 17)					\
  MEMCLR_DEFINE_ONE (align, offset, 18)					\
  MEMCLR_DEFINE_ONE (align, offset, 19)					\
  MEMCLR_DEFINE_ONE (align, offset, 20)					\
  MEMCLR_DEFINE_ONE (align, offset, 21)					\
  MEMCLR_DEFINE_ONE (align, offset, 22)					\
  MEMCLR_DEFINE_ONE (align, offset, 23)					\
  MEMCLR_DEFINE_ONE (align, offset, 24)					\
  MEMCLR_DEFINE_ONE (align, offset, 25)					\
  MEMCLR_DEFINE_ONE (align, offset, 26)					\
  MEMCLR_DEFINE_ONE (align, offset, 27)					\
  MEMCLR_DEFINE_ONE (align, offset, 28)					\
  MEMCLR_DEFINE_ONE (align, offset, 29)					\
  MEMCLR_DEFINE_ONE (align, offset, 30)					\
  MEMCLR_DEFINE_ONE (align, offset, 31)					\
  MEMCLR_DEFINE_ONE (align, offset, 32)					\
  MEMCLR_DEFINE_ONE (align, offset, 33)					\
  MEMCLR_DEFINE_ONE (align, offset, 34)					\
  MEMCLR_DEFINE_ONE (align, offset, 35)					\
  MEMCLR_DEFINE_ONE (align, offset, 36)					\
  MEMCLR_DEFINE_ONE (align, offset, 37)					\
  MEMCLR_DEFINE_ONE (align, offset, 38)					\
  MEMCLR_DEFINE_ONE (align, offset, 39)					\
  MEMCLR_DEFINE_ONE (align, offset, 40)					\
  MEMCLR_DEFINE_ONE (align, offset, 41)					\
  MEMCLR_DEFINE_ONE (align, offset, 42)					\
  MEMCLR_DEFINE_ONE (align, offset, 43)					\
  MEMCLR_DEFINE_ONE (align, offset, 44)					\
  MEMCLR_DEFINE_ONE (align, offset, 45)					\
  MEMCLR_DEFINE_ONE (align, offset, 46)					\
  MEMCLR_DEFINE_ONE (align, offset, 47)					\
  MEMCLR_DEFINE_ONE (align, offset, 48)					\
  MEMCLR_DEFINE_ONE (align, offset, 49)					\
  MEMCLR_DEFINE_ONE (align, offset, 50)					\
  MEMCLR_DEFINE_ONE (align, offset, 51)					\
  MEMCLR_DEFINE_ONE (align, offset, 52)					\
  MEMCLR_DEFINE_ONE (align, offset, 53)					\
  MEMCLR_DEFINE_ONE (align, offset, 54)					\
  MEMCLR_DEFINE_ONE (align, offset, 55)					\
  MEMCLR_DEFINE_ONE (align, offset, 56)					\
  MEMCLR_DEFINE_ONE (align, offset, 57)					\
  MEMCLR_DEFINE_ONE (align, offset, 58)					\
  MEMCLR_DEFINE_ONE (align, offset, 59)					\
  MEMCLR_DEFINE_ONE (align, offset, 60)					\
  MEMCLR_DEFINE_ONE (align, offset, 61)					\
  MEMCLR_DEFINE_ONE (align, offset, 62)					\
  MEMCLR_DEFINE_ONE (align, offset, 63)					\
  MEMCLR_DEFINE_ONE (align, offset, 64)

#define MEMCLR_DEFINE_ONE_ALIGN(align)					\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 0)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 1)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 2)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 3)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 4)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 5)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 6)				\
  MEMCLR_DEFINE_ONE_ALIGN_OFFSET (align, 7)

MEMCLR_DEFINE_ONE_ALIGN (1)
MEMCLR_DEFINE_ONE_ALIGN (2)
MEMCLR_DEFINE_ONE_ALIGN (4)
MEMCLR_DEFINE_ONE_ALIGN (8)

#define MEMCLR_CHECK_ONE(align, offset, count)				\
  memclr_check_one_ ## align ## offset ## count ();

#define MEMCLR_CHECK_ONE_ALIGN_OFFSET(align, offset)			\
  do									\
    {									\
      MEMCLR_CHECK_ONE (align, offset,  1);				\
      MEMCLR_CHECK_ONE (align, offset,  2);				\
      MEMCLR_CHECK_ONE (align, offset,  3);				\
      MEMCLR_CHECK_ONE (align, offset,  4);				\
      MEMCLR_CHECK_ONE (align, offset,  5);				\
      MEMCLR_CHECK_ONE (align, offset,  6);				\
      MEMCLR_CHECK_ONE (align, offset,  7);				\
      MEMCLR_CHECK_ONE (align, offset,  8);				\
      MEMCLR_CHECK_ONE (align, offset,  9);				\
      MEMCLR_CHECK_ONE (align, offset, 10);				\
      MEMCLR_CHECK_ONE (align, offset, 11);				\
      MEMCLR_CHECK_ONE (align, offset, 12);				\
      MEMCLR_CHECK_ONE (align, offset, 13);				\
      MEMCLR_CHECK_ONE (align, offset, 14);				\
      MEMCLR_CHECK_ONE (align, offset, 15);				\
      MEMCLR_CHECK_ONE (align, offset, 16);				\
      MEMCLR_CHECK_ONE (align, offset, 17);				\
      MEMCLR_CHECK_ONE (align, offset, 18);				\
      MEMCLR_CHECK_ONE (align, offset, 19);				\
      MEMCLR_CHECK_ONE (align, offset, 20);				\
      MEMCLR_CHECK_ONE (align, offset, 21);				\
      MEMCLR_CHECK_ONE (align, offset, 22);				\
      MEMCLR_CHECK_ONE (align, offset, 23);				\
      MEMCLR_CHECK_ONE (align, offset, 24);				\
      MEMCLR_CHECK_ONE (align, offset, 25);				\
      MEMCLR_CHECK_ONE (align, offset, 26);				\
      MEMCLR_CHECK_ONE (align, offset, 27);				\
      MEMCLR_CHECK_ONE (align, offset, 28);				\
      MEMCLR_CHECK_ONE (align, offset, 29);				\
      MEMCLR_CHECK_ONE (align, offset, 30);				\
      MEMCLR_CHECK_ONE (align, offset, 31);				\
      MEMCLR_CHECK_ONE (align, offset, 32);				\
      MEMCLR_CHECK_ONE (align, offset, 33);				\
      MEMCLR_CHECK_ONE (align, offset, 34);				\
      MEMCLR_CHECK_ONE (align, offset, 35);				\
      MEMCLR_CHECK_ONE (align, offset, 36);				\
      MEMCLR_CHECK_ONE (align, offset, 37);				\
      MEMCLR_CHECK_ONE (align, offset, 38);				\
      MEMCLR_CHECK_ONE (align, offset, 39);				\
      MEMCLR_CHECK_ONE (align, offset, 40);				\
      MEMCLR_CHECK_ONE (align, offset, 41);				\
      MEMCLR_CHECK_ONE (align, offset, 42);				\
      MEMCLR_CHECK_ONE (align, offset, 43);				\
      MEMCLR_CHECK_ONE (align, offset, 44);				\
      MEMCLR_CHECK_ONE (align, offset, 45);				\
      MEMCLR_CHECK_ONE (align, offset, 46);				\
      MEMCLR_CHECK_ONE (align, offset, 47);				\
      MEMCLR_CHECK_ONE (align, offset, 48);				\
      MEMCLR_CHECK_ONE (align, offset, 49);				\
      MEMCLR_CHECK_ONE (align, offset, 50);				\
      MEMCLR_CHECK_ONE (align, offset, 51);				\
      MEMCLR_CHECK_ONE (align, offset, 52);				\
      MEMCLR_CHECK_ONE (align, offset, 53);				\
      MEMCLR_CHECK_ONE (align, offset, 54);				\
      MEMCLR_CHECK_ONE (align, offset, 55);				\
      MEMCLR_CHECK_ONE (align, offset, 56);				\
      MEMCLR_CHECK_ONE (align, offset, 57);				\
      MEMCLR_CHECK_ONE (align, offset, 58);				\
      MEMCLR_CHECK_ONE (align, offset, 59);				\
      MEMCLR_CHECK_ONE (align, offset, 60);				\
      MEMCLR_CHECK_ONE (align, offset, 61);				\
      MEMCLR_CHECK_ONE (align, offset, 62);				\
      MEMCLR_CHECK_ONE (align, offset, 63);				\
      MEMCLR_CHECK_ONE (align, offset, 64);				\
    }									\
  while (0);

#define MEMCLR_CHECK_ONE_ALIGN(align)					\
  do									\
    {									\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 0);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 1);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 2);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 3);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 4);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 5);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 6);				\
      MEMCLR_CHECK_ONE_ALIGN_OFFSET (align, 7);				\
    }									\
  while (0);

int
main (void)
{
  MEMCLR_CHECK_ONE_ALIGN (1);
  MEMCLR_CHECK_ONE_ALIGN (2);
  MEMCLR_CHECK_ONE_ALIGN (4);
  MEMCLR_CHECK_ONE_ALIGN (8);
  return 0;
}
