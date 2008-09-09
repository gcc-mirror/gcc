#include <stdarg.h>

#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#define T(NAME, TYPE, INITVAL)					\
extern TYPE g01##NAME;						\
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
}

#ifndef SKIP_COMPLEX
#ifndef SKIP_COMPLEX_INT
T(cc, _Complex char, CINT (0, 1))
T(cs, _Complex short, CINT (1, 2))
#endif
T(cf, _Complex float, CDBL (1.0, 2.0))
#endif
