#include <stdarg.h>

#include "compat-common.h"

#include "union-defs.h"
#include "union-init.h"

#define T(TYPE)							\
extern TYPE g01##TYPE, g02##TYPE, g03##TYPE, g04##TYPE;		\
extern TYPE g05##TYPE, g06##TYPE, g07##TYPE, g08##TYPE;		\
extern TYPE g09##TYPE, g10##TYPE, g11##TYPE, g12##TYPE;		\
extern TYPE g13##TYPE, g14##TYPE, g15##TYPE, g16##TYPE;		\
								\
extern void check##TYPE (TYPE x, int i);			\
								\
void								\
checkg##TYPE (void)						\
{								\
  check##TYPE (g01##TYPE,  1);					\
  check##TYPE (g02##TYPE,  2);					\
  check##TYPE (g03##TYPE,  3);					\
  check##TYPE (g04##TYPE,  4);					\
  check##TYPE (g05##TYPE,  5);					\
  check##TYPE (g06##TYPE,  6);					\
  check##TYPE (g07##TYPE,  7);					\
  check##TYPE (g08##TYPE,  8);					\
  check##TYPE (g09##TYPE,  9);					\
  check##TYPE (g10##TYPE, 10);					\
  check##TYPE (g11##TYPE, 11);					\
  check##TYPE (g12##TYPE, 12);					\
  check##TYPE (g13##TYPE, 13);					\
  check##TYPE (g14##TYPE, 14);					\
  check##TYPE (g15##TYPE, 15);					\
  check##TYPE (g16##TYPE, 16);					\
}								\
								\
TYPE								\
test0##TYPE (void)						\
{								\
  return g01##TYPE;						\
}								\
								\
TYPE								\
test1##TYPE (TYPE x01)						\
{								\
  return x01;							\
}								\
								\
TYPE								\
testva##TYPE (int n, ...)					\
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

T(Ucs)
T(Uci)
T(Ucl)
T(Ucll)
T(Usi)
T(Usl)
T(Usll)
T(Uil)
T(Uill)
T(Ulll)
