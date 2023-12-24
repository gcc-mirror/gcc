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
}

MAKE_FUNS (, unsigned);

extern void abort (void);
extern void exit (int);


int
main (void)
{
  check_vect ();

#define TEST(x, suffix)							\
  if (__builtin_clrsb##suffix (x) != my_clrsb##suffix (x))		\
    abort ();								

#if BITSIZEOF_INT == 32
  TEST(0xffffffffUL,);
#endif
  exit (0);
}
