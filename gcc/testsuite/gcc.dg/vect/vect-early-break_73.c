/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "x86_64-*-* i?86-*-*" } } } } */

#include <limits.h>
#include <assert.h>

#include "tree-vect.h"

#  define BITSIZEOF_INT 32
#  define BITSIZEOF_LONG 64
#  define BITSIZEOF_LONG_LONG 64

#define MAKE_FUNS(suffix, type)						\
__attribute__((noinline)) \
int my_clz##suffix(type x) {						\
    int i;								\
    for (i = 0; i < CHAR_BIT * sizeof (type); i++)			\
	if (x & ((type) 1 << ((CHAR_BIT * sizeof (type)) - i - 1)))	\
	    break;							\
    return i;								\
}


MAKE_FUNS (, unsigned);

extern void abort (void);
extern void exit (int);

#define NUMS32					\
  {                                             \
    0x00000000UL,                               \
    0x00000001UL,                               \
    0x80000000UL,                               \
    0x00000002UL,                               \
    0x40000000UL,                               \
    0x00010000UL,                               \
    0x00008000UL,                               \
    0xa5a5a5a5UL,                               \
    0x5a5a5a5aUL,                               \
    0xcafe0000UL,                               \
    0x00cafe00UL,                               \
    0x0000cafeUL,                               \
    0xffffffffUL                                \
  }


unsigned int ints[] = NUMS32;

#define N(table) (sizeof (table) / sizeof (table[0]))

int
main (void)
{
  int i;

  check_vect ();

#pragma GCC novector
  for (i = 0; i < N(ints); i++)
    {
      if (ints[i] != 0
	  && __builtin_clz (ints[i]) != my_clz (ints[i]))
	  abort ();
    }

  exit (0);
}

