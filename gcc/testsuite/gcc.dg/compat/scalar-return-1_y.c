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
void								\
init##NAME (TYPE *p, TYPE v)					\
{								\
  *p = v + INITVAL;						\
}								\
								\
void								\
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
TYPE								\
test0##NAME (void)						\
{								\
  return g01##NAME;						\
}								\
								\
TYPE								\
test1##NAME (TYPE x01)						\
{								\
  return x01;							\
}								\
								\
TYPE								\
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

T(ui, unsigned int, 51)
T(si, int, (-55))
T(ul, unsigned long, 61)
T(sl, long, (-66))
T(ull, unsigned long long, 71)
T(sll, long long, (-77))
T(d, double, 91.0)
T(ld, long double, 92.0)
