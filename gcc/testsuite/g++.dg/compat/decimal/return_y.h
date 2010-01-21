#include <stdarg.h>

#include "compat-common.h"

#define T(NAME, TYPE, INITVAL)					\
extern TYPE g01##NAME, g02##NAME, g03##NAME, g04##NAME;		\
extern TYPE g05##NAME, g06##NAME, g07##NAME, g08##NAME;		\
extern TYPE g09##NAME, g10##NAME, g11##NAME, g12##NAME;		\
extern TYPE g13##NAME, g14##NAME, g15##NAME, g16##NAME;		\
								\
extern void check##NAME (TYPE x, TYPE v);			\
								\
extern void							\
init##NAME (TYPE *p, TYPE v)					\
{								\
  *p = v + INITVAL;						\
}								\
								\
extern void							\
checkg##NAME (void)						\
{								\
  check##NAME (g01##NAME,  1+INITVAL);				\
  check##NAME (g02##NAME,  2+INITVAL);				\
  check##NAME (g03##NAME,  3+INITVAL);				\
  check##NAME (g04##NAME,  4+INITVAL);				\
  check##NAME (g05##NAME,  5+INITVAL);				\
  check##NAME (g06##NAME,  6+INITVAL);				\
  check##NAME (g07##NAME,  7+INITVAL);				\
  check##NAME (g08##NAME,  8+INITVAL);				\
  check##NAME (g09##NAME,  9+INITVAL);				\
  check##NAME (g10##NAME, 10+INITVAL);				\
  check##NAME (g11##NAME, 11+INITVAL);				\
  check##NAME (g12##NAME, 12+INITVAL);				\
  check##NAME (g13##NAME, 13+INITVAL);				\
  check##NAME (g14##NAME, 14+INITVAL);				\
  check##NAME (g15##NAME, 15+INITVAL);				\
  check##NAME (g16##NAME, 16+INITVAL);				\
}								\
								\
extern TYPE							\
test0##NAME (void)						\
{								\
  return g01##NAME;						\
}								\
								\
extern TYPE							\
test1##NAME (TYPE x01)						\
{								\
  return x01;							\
}								\
								\
extern TYPE							\
testva##NAME (int n, ...)					\
{								\
  int i;							\
  TYPE rslt;							\
  va_list ap;							\
  va_start (ap, n);						\
  for (i = 0; i < n; i++)					\
    rslt = va_arg (ap, TYPE);					\
  va_end (ap);							\
  return rslt;							\
}

T(d32, dec32, (dec32)1.5)
T(d64, dec64, (dec64)2.5)
T(d128, dec128, (dec128)3.5)
