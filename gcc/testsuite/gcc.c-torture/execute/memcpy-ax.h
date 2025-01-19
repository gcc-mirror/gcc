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

ax_t src = {{
  0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
  0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10,
  0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
  0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20,
  0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
  0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30,
  0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
  0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x70,
  0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
  0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50,
  0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
}};

#define MEMCPY_DEFINE_ONE(align, offset, count)				\
  static void __attribute__ ((noinline))				\
  memcpy_check_one_ ## align ## offset ## count (void)			\
    {									\
      static a ## align ## _t dst = {{ [0 ... 87] = 0xaa }};		\
      int i;								\
									\
      __builtin_memcpy (dst.v + 8 + offset, src.v + 8 + offset, count);	\
      asm ("" : : : "memory");						\
      for (i = 0; i < 8 + offset; i++)					\
	if (dst.v[i] != 0xaa)						\
	  __builtin_abort ();						\
      for (; i < 8 + offset + count; i++)				\
	if (dst.v[i] != src.v[i])					\
	  __builtin_abort ();						\
      for (; i < sizeof (dst.v); i++)					\
	if (dst.v[i] != 0xaa)						\
	  __builtin_abort ();						\
    }

#define MEMCPY_DEFINE_ONE_ALIGN_OFFSET(align, offset)			\
  MEMCPY_DEFINE_ONE (align, offset,  1)					\
  MEMCPY_DEFINE_ONE (align, offset,  2)					\
  MEMCPY_DEFINE_ONE (align, offset,  3)					\
  MEMCPY_DEFINE_ONE (align, offset,  4)					\
  MEMCPY_DEFINE_ONE (align, offset,  5)					\
  MEMCPY_DEFINE_ONE (align, offset,  6)					\
  MEMCPY_DEFINE_ONE (align, offset,  7)					\
  MEMCPY_DEFINE_ONE (align, offset,  8)					\
  MEMCPY_DEFINE_ONE (align, offset,  9)					\
  MEMCPY_DEFINE_ONE (align, offset, 10)					\
  MEMCPY_DEFINE_ONE (align, offset, 11)					\
  MEMCPY_DEFINE_ONE (align, offset, 12)					\
  MEMCPY_DEFINE_ONE (align, offset, 13)					\
  MEMCPY_DEFINE_ONE (align, offset, 14)					\
  MEMCPY_DEFINE_ONE (align, offset, 15)					\
  MEMCPY_DEFINE_ONE (align, offset, 16)					\
  MEMCPY_DEFINE_ONE (align, offset, 17)					\
  MEMCPY_DEFINE_ONE (align, offset, 18)					\
  MEMCPY_DEFINE_ONE (align, offset, 19)					\
  MEMCPY_DEFINE_ONE (align, offset, 20)					\
  MEMCPY_DEFINE_ONE (align, offset, 21)					\
  MEMCPY_DEFINE_ONE (align, offset, 22)					\
  MEMCPY_DEFINE_ONE (align, offset, 23)					\
  MEMCPY_DEFINE_ONE (align, offset, 24)					\
  MEMCPY_DEFINE_ONE (align, offset, 25)					\
  MEMCPY_DEFINE_ONE (align, offset, 26)					\
  MEMCPY_DEFINE_ONE (align, offset, 27)					\
  MEMCPY_DEFINE_ONE (align, offset, 28)					\
  MEMCPY_DEFINE_ONE (align, offset, 29)					\
  MEMCPY_DEFINE_ONE (align, offset, 30)					\
  MEMCPY_DEFINE_ONE (align, offset, 31)					\
  MEMCPY_DEFINE_ONE (align, offset, 32)					\
  MEMCPY_DEFINE_ONE (align, offset, 33)					\
  MEMCPY_DEFINE_ONE (align, offset, 34)					\
  MEMCPY_DEFINE_ONE (align, offset, 35)					\
  MEMCPY_DEFINE_ONE (align, offset, 36)					\
  MEMCPY_DEFINE_ONE (align, offset, 37)					\
  MEMCPY_DEFINE_ONE (align, offset, 38)					\
  MEMCPY_DEFINE_ONE (align, offset, 39)					\
  MEMCPY_DEFINE_ONE (align, offset, 40)					\
  MEMCPY_DEFINE_ONE (align, offset, 41)					\
  MEMCPY_DEFINE_ONE (align, offset, 42)					\
  MEMCPY_DEFINE_ONE (align, offset, 43)					\
  MEMCPY_DEFINE_ONE (align, offset, 44)					\
  MEMCPY_DEFINE_ONE (align, offset, 45)					\
  MEMCPY_DEFINE_ONE (align, offset, 46)					\
  MEMCPY_DEFINE_ONE (align, offset, 47)					\
  MEMCPY_DEFINE_ONE (align, offset, 48)					\
  MEMCPY_DEFINE_ONE (align, offset, 49)					\
  MEMCPY_DEFINE_ONE (align, offset, 50)					\
  MEMCPY_DEFINE_ONE (align, offset, 51)					\
  MEMCPY_DEFINE_ONE (align, offset, 52)					\
  MEMCPY_DEFINE_ONE (align, offset, 53)					\
  MEMCPY_DEFINE_ONE (align, offset, 54)					\
  MEMCPY_DEFINE_ONE (align, offset, 55)					\
  MEMCPY_DEFINE_ONE (align, offset, 56)					\
  MEMCPY_DEFINE_ONE (align, offset, 57)					\
  MEMCPY_DEFINE_ONE (align, offset, 58)					\
  MEMCPY_DEFINE_ONE (align, offset, 59)					\
  MEMCPY_DEFINE_ONE (align, offset, 60)					\
  MEMCPY_DEFINE_ONE (align, offset, 61)					\
  MEMCPY_DEFINE_ONE (align, offset, 62)					\
  MEMCPY_DEFINE_ONE (align, offset, 63)					\
  MEMCPY_DEFINE_ONE (align, offset, 64)

#define MEMCPY_DEFINE_ONE_ALIGN(align)					\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 0)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 1)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 2)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 3)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 4)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 5)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 6)				\
  MEMCPY_DEFINE_ONE_ALIGN_OFFSET (align, 7)

MEMCPY_DEFINE_ONE_ALIGN (1)
MEMCPY_DEFINE_ONE_ALIGN (2)
MEMCPY_DEFINE_ONE_ALIGN (4)
MEMCPY_DEFINE_ONE_ALIGN (8)

#define MEMCPY_CHECK_ONE(align, offset, count)				\
  memcpy_check_one_ ## align ## offset ## count ();

#define MEMCPY_CHECK_ONE_ALIGN_OFFSET(align, offset)			\
  do									\
    {									\
      MEMCPY_CHECK_ONE (align, offset,  1);				\
      MEMCPY_CHECK_ONE (align, offset,  2);				\
      MEMCPY_CHECK_ONE (align, offset,  3);				\
      MEMCPY_CHECK_ONE (align, offset,  4);				\
      MEMCPY_CHECK_ONE (align, offset,  5);				\
      MEMCPY_CHECK_ONE (align, offset,  6);				\
      MEMCPY_CHECK_ONE (align, offset,  7);				\
      MEMCPY_CHECK_ONE (align, offset,  8);				\
      MEMCPY_CHECK_ONE (align, offset,  9);				\
      MEMCPY_CHECK_ONE (align, offset, 10);				\
      MEMCPY_CHECK_ONE (align, offset, 11);				\
      MEMCPY_CHECK_ONE (align, offset, 12);				\
      MEMCPY_CHECK_ONE (align, offset, 13);				\
      MEMCPY_CHECK_ONE (align, offset, 14);				\
      MEMCPY_CHECK_ONE (align, offset, 15);				\
      MEMCPY_CHECK_ONE (align, offset, 16);				\
      MEMCPY_CHECK_ONE (align, offset, 17);				\
      MEMCPY_CHECK_ONE (align, offset, 18);				\
      MEMCPY_CHECK_ONE (align, offset, 19);				\
      MEMCPY_CHECK_ONE (align, offset, 20);				\
      MEMCPY_CHECK_ONE (align, offset, 21);				\
      MEMCPY_CHECK_ONE (align, offset, 22);				\
      MEMCPY_CHECK_ONE (align, offset, 23);				\
      MEMCPY_CHECK_ONE (align, offset, 24);				\
      MEMCPY_CHECK_ONE (align, offset, 25);				\
      MEMCPY_CHECK_ONE (align, offset, 26);				\
      MEMCPY_CHECK_ONE (align, offset, 27);				\
      MEMCPY_CHECK_ONE (align, offset, 28);				\
      MEMCPY_CHECK_ONE (align, offset, 29);				\
      MEMCPY_CHECK_ONE (align, offset, 30);				\
      MEMCPY_CHECK_ONE (align, offset, 31);				\
      MEMCPY_CHECK_ONE (align, offset, 32);				\
      MEMCPY_CHECK_ONE (align, offset, 33);				\
      MEMCPY_CHECK_ONE (align, offset, 34);				\
      MEMCPY_CHECK_ONE (align, offset, 35);				\
      MEMCPY_CHECK_ONE (align, offset, 36);				\
      MEMCPY_CHECK_ONE (align, offset, 37);				\
      MEMCPY_CHECK_ONE (align, offset, 38);				\
      MEMCPY_CHECK_ONE (align, offset, 39);				\
      MEMCPY_CHECK_ONE (align, offset, 40);				\
      MEMCPY_CHECK_ONE (align, offset, 41);				\
      MEMCPY_CHECK_ONE (align, offset, 42);				\
      MEMCPY_CHECK_ONE (align, offset, 43);				\
      MEMCPY_CHECK_ONE (align, offset, 44);				\
      MEMCPY_CHECK_ONE (align, offset, 45);				\
      MEMCPY_CHECK_ONE (align, offset, 46);				\
      MEMCPY_CHECK_ONE (align, offset, 47);				\
      MEMCPY_CHECK_ONE (align, offset, 48);				\
      MEMCPY_CHECK_ONE (align, offset, 49);				\
      MEMCPY_CHECK_ONE (align, offset, 50);				\
      MEMCPY_CHECK_ONE (align, offset, 51);				\
      MEMCPY_CHECK_ONE (align, offset, 52);				\
      MEMCPY_CHECK_ONE (align, offset, 53);				\
      MEMCPY_CHECK_ONE (align, offset, 54);				\
      MEMCPY_CHECK_ONE (align, offset, 55);				\
      MEMCPY_CHECK_ONE (align, offset, 56);				\
      MEMCPY_CHECK_ONE (align, offset, 57);				\
      MEMCPY_CHECK_ONE (align, offset, 58);				\
      MEMCPY_CHECK_ONE (align, offset, 59);				\
      MEMCPY_CHECK_ONE (align, offset, 60);				\
      MEMCPY_CHECK_ONE (align, offset, 61);				\
      MEMCPY_CHECK_ONE (align, offset, 62);				\
      MEMCPY_CHECK_ONE (align, offset, 63);				\
      MEMCPY_CHECK_ONE (align, offset, 64);				\
    }									\
  while (0);

#define MEMCPY_CHECK_ONE_ALIGN(align)					\
  do									\
    {									\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 0);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 1);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 2);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 3);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 4);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 5);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 6);				\
      MEMCPY_CHECK_ONE_ALIGN_OFFSET (align, 7);				\
    }									\
  while (0);

int
main (void)
{
  MEMCPY_CHECK_ONE_ALIGN (1);
  MEMCPY_CHECK_ONE_ALIGN (2);
  MEMCPY_CHECK_ONE_ALIGN (4);
  MEMCPY_CHECK_ONE_ALIGN (8);
  return 0;
}
