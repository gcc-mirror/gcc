/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-O3" } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "x86_64-*-* i?86-*-*" } } } } */

#include <limits.h>
#include <assert.h>

#include "tree-vect.h"

#if __INT_MAX__ > 2147483647L
# if __INT_MAX__ >= 9223372036854775807L
#  define BITSIZEOF_INT 64
# else
#  define BITSIZEOF_INT 32
# endif
#else
# if __INT_MAX__ >= 2147483647L
#  define BITSIZEOF_INT 32
# else
#  define BITSIZEOF_INT 16
# endif
#endif

#if __LONG_MAX__ > 2147483647L
# if __LONG_MAX__ >= 9223372036854775807L
#  define BITSIZEOF_LONG 64
# else
#  define BITSIZEOF_LONG 32
# endif
#else
# define BITSIZEOF_LONG 32
#endif

#if __LONG_LONG_MAX__ > 2147483647L
# if __LONG_LONG_MAX__ >= 9223372036854775807L
#  define BITSIZEOF_LONG_LONG 64
# else
#  define BITSIZEOF_LONG_LONG 32
# endif
#else
# define BITSIZEOF_LONG_LONG 32
#endif

#define MAKE_FUNS(suffix, type)						\
int my_clrsb##suffix(type x) {						\
    int i;								\
    int leading = (x >> CHAR_BIT * sizeof (type) - 1) & 1;		\
    for (i = 1; i < CHAR_BIT * sizeof (type); i++)			\
	if (((x >> ((CHAR_BIT * sizeof (type)) - i - 1)) & 1)		\
	    != leading)							\
	    break;							\
    return i - 1;							\
}									\
									\

MAKE_FUNS (, unsigned);
MAKE_FUNS (ll, unsigned long long);

extern void abort (void);
extern void exit (int);

#define NUMS16					\
  {						\
    0x0000U,					\
    0x0001U,					\
    0x8000U,					\
    0x0002U,					\
    0x4000U,					\
    0x0100U,					\
    0x0080U,					\
    0xa5a5U,					\
    0x5a5aU,					\
    0xcafeU,					\
    0xffffU					\
  }

#define NUMS32					\
  {						\
    0x00000000UL,				\
    0x00000001UL,				\
    0x80000000UL,				\
    0x00000002UL,				\
    0x40000000UL,				\
    0x00010000UL,				\
    0x00008000UL,				\
    0xa5a5a5a5UL,				\
    0x5a5a5a5aUL,				\
    0xcafe0000UL,				\
    0x00cafe00UL,				\
    0x0000cafeUL,				\
    0xffffffffUL				\
  }

#define NUMS64					\
  {						\
    0x0000000000000000ULL,			\
    0x0000000000000001ULL,			\
    0x8000000000000000ULL,			\
    0x0000000000000002ULL,			\
    0x4000000000000000ULL,			\
    0x0000000100000000ULL,			\
    0x0000000080000000ULL,			\
    0xa5a5a5a5a5a5a5a5ULL,			\
    0x5a5a5a5a5a5a5a5aULL,			\
    0xcafecafe00000000ULL,			\
    0x0000cafecafe0000ULL,			\
    0x00000000cafecafeULL,			\
    0xffffffffffffffffULL			\
  }

unsigned int ints[] =
#if BITSIZEOF_INT == 64
NUMS64;
#elif BITSIZEOF_INT == 32
NUMS32;
#else
NUMS16;
#endif

unsigned long longs[] =
#if BITSIZEOF_LONG == 64
NUMS64;
#else
NUMS32;
#endif

unsigned long long longlongs[] =
#if BITSIZEOF_LONG_LONG == 64
NUMS64;
#else
NUMS32;
#endif

#define N(table) (sizeof (table) / sizeof (table[0]))

int
main (void)
{
  int i;

  check_vect ();

#pragma GCC novector
  for (i = 0; i < N(ints); i++)
    {
      if (__builtin_clrsb (ints[i]) != my_clrsb (ints[i]))
	abort ();
    }

  /* Test constant folding.  */

#define TEST(x, suffix)							\
  if (__builtin_clrsb##suffix (x) != my_clrsb##suffix (x))		\
    abort ();								

#if BITSIZEOF_LONG_LONG == 64
  TEST(0xffffffffffffffffULL, ll);
  TEST(0xffffffffffffffffULL, ll);
  TEST(0xffffffffffffffffULL, ll);
  TEST(0xffffffffffffffffULL, ll);
  TEST(0xffffffffffffffffULL, ll);
  TEST(0xffffffffffffffffULL, ll);
#endif

  exit (0);
}
