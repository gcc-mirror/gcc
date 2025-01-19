typedef unsigned int __attribute__ ((mode (QI))) intw1_t;
typedef unsigned int __attribute__ ((mode (HI))) intw2_t;
typedef unsigned int __attribute__ ((mode (SI))) intw4_t;
typedef unsigned int __attribute__ ((mode (DI))) intw8_t;

#define MISALIGN_DEFINE_ONE(align, width, offset)			\
  static void								\
  misalign_check_one_ ## align ## width ## offset (void)		\
    {									\
      static union							\
	{								\
	  intw1_t v[32];						\
	  struct __attribute__ ((packed))				\
	    {								\
	      intw1_t o[8 + offset];					\
	      intw ## width ## _t x;					\
	    } x;							\
	  intw ## align ## _t a;					\
	}								\
      dst = {{ [0 ... 31] = 0xaa }};					\
      static const union						\
	{								\
	  intw1_t v[8];							\
	  intw ## width ## _t x;					\
	}								\
      src = {{ 1, 2, 3, 4, 5, 6, 7, 8 }};				\
      int i, j;								\
									\
      dst.x.x = src.x;							\
      asm ("" : : : "memory");						\
      for (i = 0; i < 8 + offset; i++)					\
	if (dst.v[i] != 0xaa)						\
	  __builtin_abort ();						\
      for (j = 0; i < 8 + offset + width; i++, j++)			\
	if (dst.v[i] != src.v[j])					\
	  __builtin_abort ();						\
      for (; i < sizeof (dst.v); i++)					\
	if (dst.v[i] != 0xaa)						\
	  __builtin_abort ();						\
    }

#define MISALIGN_DEFINE_ONE_ALIGN_WIDTH(align, width)			\
  MISALIGN_DEFINE_ONE (align, width, 1)					\
  MISALIGN_DEFINE_ONE (align, width, 2)					\
  MISALIGN_DEFINE_ONE (align, width, 3)					\
  MISALIGN_DEFINE_ONE (align, width, 4)					\
  MISALIGN_DEFINE_ONE (align, width, 5)					\
  MISALIGN_DEFINE_ONE (align, width, 6)					\
  MISALIGN_DEFINE_ONE (align, width, 7)

MISALIGN_DEFINE_ONE_ALIGN_WIDTH (1, 2)
MISALIGN_DEFINE_ONE_ALIGN_WIDTH (1, 4)
MISALIGN_DEFINE_ONE_ALIGN_WIDTH (1, 8)
MISALIGN_DEFINE_ONE_ALIGN_WIDTH (2, 4)
MISALIGN_DEFINE_ONE_ALIGN_WIDTH (2, 8)
MISALIGN_DEFINE_ONE_ALIGN_WIDTH (4, 8)

#define MISALIGN_CHECK_ONE(align, width, offset)			\
  misalign_check_one_ ## align ## width ## offset ();

#define MISALIGN_CHECK_ONE_ALIGN_WIDTH(align, width)			\
  do									\
    {									\
      MISALIGN_CHECK_ONE (align, width, 1);				\
      MISALIGN_CHECK_ONE (align, width, 2);				\
      MISALIGN_CHECK_ONE (align, width, 3);				\
      MISALIGN_CHECK_ONE (align, width, 4);				\
      MISALIGN_CHECK_ONE (align, width, 5);				\
      MISALIGN_CHECK_ONE (align, width, 6);				\
      MISALIGN_CHECK_ONE (align, width, 7);				\
    }									\
  while (0);

int
main (void)
{
  MISALIGN_CHECK_ONE_ALIGN_WIDTH (1, 2);
  MISALIGN_CHECK_ONE_ALIGN_WIDTH (1, 4);
  MISALIGN_CHECK_ONE_ALIGN_WIDTH (1, 8);
  MISALIGN_CHECK_ONE_ALIGN_WIDTH (2, 4);
  MISALIGN_CHECK_ONE_ALIGN_WIDTH (2, 8);
  MISALIGN_CHECK_ONE_ALIGN_WIDTH (4, 8);
  return 0;
}
