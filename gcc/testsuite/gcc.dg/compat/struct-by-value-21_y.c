#include <stdarg.h>

#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "mixed-struct-defs.h"
#include "mixed-struct-init.h"

#define T(TYPE)							\
extern void check##TYPE (TYPE x, int i);			\
extern TYPE g1s##TYPE, g2s##TYPE, g3s##TYPE, g4s##TYPE;		\
extern TYPE g5s##TYPE, g6s##TYPE, g7s##TYPE, g8s##TYPE;		\
extern TYPE g9s##TYPE, g10s##TYPE, g11s##TYPE, g12s##TYPE;	\
extern TYPE g13s##TYPE, g14s##TYPE, g15s##TYPE, g16s##TYPE;	\
								\
void								\
checkg##TYPE (void)						\
{								\
  check##TYPE (  g1s##TYPE,  1);				\
  check##TYPE (  g2s##TYPE,  2);				\
  check##TYPE (  g3s##TYPE,  3);				\
  check##TYPE (  g4s##TYPE,  4);				\
  check##TYPE (  g5s##TYPE,  5);				\
  check##TYPE (  g6s##TYPE,  6);				\
  check##TYPE (  g7s##TYPE,  7);				\
  check##TYPE (  g8s##TYPE,  8);				\
  check##TYPE (  g9s##TYPE,  9);				\
  check##TYPE ( g10s##TYPE, 10);				\
  check##TYPE ( g11s##TYPE, 11);				\
  check##TYPE ( g12s##TYPE, 12);				\
  check##TYPE ( g13s##TYPE, 13);				\
  check##TYPE ( g14s##TYPE, 14);				\
  check##TYPE ( g15s##TYPE, 15);				\
  check##TYPE ( g16s##TYPE, 16);				\
}								\
								\
void								\
test##TYPE (TYPE s1, TYPE s2, TYPE s3, TYPE s4,			\
	    TYPE s5, TYPE s6, TYPE s7, TYPE s8,			\
	    TYPE s9, TYPE s10, TYPE s11, TYPE s12,		\
	    TYPE s13, TYPE s14, TYPE s15, TYPE s16)		\
{								\
  check##TYPE (s1, 1);						\
  check##TYPE (s2, 2);						\
  check##TYPE (s3, 3);						\
  check##TYPE (s4, 4);						\
  check##TYPE (s5, 5);						\
  check##TYPE (s6, 6);						\
  check##TYPE (s7, 7);						\
  check##TYPE (s8, 8);						\
  check##TYPE (s9, 9);						\
  check##TYPE (s10, 10);					\
  check##TYPE (s11, 11);					\
  check##TYPE (s12, 12);					\
  check##TYPE (s13, 13);					\
  check##TYPE (s14, 14);					\
  check##TYPE (s15, 15);					\
  check##TYPE (s16, 16);					\
}								\
								\
void								\
testva##TYPE (int n, ...)					\
{								\
  int i;							\
  va_list ap;							\
  if (test_va)							\
    {								\
      va_start (ap, n);						\
      for (i = 0; i < n; i++)					\
	{							\
	  TYPE t = va_arg (ap, TYPE);				\
	  check##TYPE (t, i+1);					\
	}							\
      va_end (ap);						\
    }								\
}

T(Sfi)
T(Sfii)
T(Sfifi)
T(Sfiifii)
