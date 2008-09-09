#include "compat-common.h"

#define T(NAME, TYPE, INITVAL) 					\
TYPE g01##NAME, g02##NAME, g03##NAME, g04##NAME;		\
TYPE g05##NAME, g06##NAME, g07##NAME, g08##NAME;		\
TYPE g09##NAME, g10##NAME, g11##NAME, g12##NAME;		\
TYPE g13##NAME, g14##NAME, g15##NAME, g16##NAME;		\
								\
extern void init##NAME (TYPE *p, TYPE v);			\
extern void checkg##NAME (void);				\
extern void							\
test##NAME (TYPE x01, TYPE x02, TYPE x03, TYPE x04,		\
            TYPE x05, TYPE x06, TYPE x07, TYPE x08,		\
            TYPE x09, TYPE x10, TYPE x11, TYPE x12,		\
            TYPE x13, TYPE x14, TYPE x15, TYPE x16);		\
								\
void								\
check##NAME (TYPE x, TYPE v)					\
{								\
  if (x != v + INITVAL)						\
    DEBUG_CHECK							\
}								\
								\
void								\
test2_##NAME (TYPE x01, TYPE x02, TYPE x03, TYPE x04,		\
	      TYPE x05, TYPE x06, TYPE x07, TYPE x08)		\
{								\
  test##NAME (x01, g02##NAME, x02, g04##NAME,			\
	      x03, g06##NAME, x04, g08##NAME,			\
	      x05, g10##NAME, x06, g12##NAME,			\
	      x07, g14##NAME, x08, g16##NAME);			\
}								\
								\
void								\
testit##NAME (void)						\
{								\
  DEBUG_FPUTS (#NAME);						\
  DEBUG_FPUTS (" init: ");					\
  init##NAME (&g01##NAME,  1);					\
  init##NAME (&g02##NAME,  2);					\
  init##NAME (&g03##NAME,  3);					\
  init##NAME (&g04##NAME,  4);					\
  init##NAME (&g05##NAME,  5);					\
  init##NAME (&g06##NAME,  6);					\
  init##NAME (&g07##NAME,  7);					\
  init##NAME (&g08##NAME,  8);					\
  init##NAME (&g09##NAME,  9);					\
  init##NAME (&g10##NAME, 10);					\
  init##NAME (&g11##NAME, 11);					\
  init##NAME (&g12##NAME, 12);					\
  init##NAME (&g13##NAME, 13);					\
  init##NAME (&g14##NAME, 14);					\
  init##NAME (&g15##NAME, 15);					\
  init##NAME (&g16##NAME, 16);					\
  checkg##NAME ();						\
  DEBUG_NL;							\
  DEBUG_FPUTS (#NAME);						\
  DEBUG_FPUTS (" test: ");					\
  test##NAME (g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
	      g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
	      g09##NAME, g10##NAME, g11##NAME, g12##NAME,	\
	      g13##NAME, g14##NAME, g15##NAME, g16##NAME);	\
  DEBUG_NL;							\
  DEBUG_FPUTS (#NAME);						\
  DEBUG_FPUTS (" test2: ");					\
  test2_##NAME (g01##NAME, g03##NAME, g05##NAME, g07##NAME,	\
		g09##NAME, g11##NAME, g13##NAME, g15##NAME);	\
  DEBUG_NL;							\
}

#ifndef SKIP_COMPLEX
#ifndef SKIP_COMPLEX_INT
T(cc, _Complex char, CINT (0, 1))
T(cs, _Complex short, CINT (1, 2))
#endif
T(cf, _Complex float, CDBL (6.0, 7.0))
#endif

#undef T

void
scalar_by_value_4_x ()
{
DEBUG_INIT

#define T(NAME) testit##NAME ();

#ifndef SKIP_COMPLEX
#ifndef SKIP_COMPLEX_INT
T(cc)
T(cs)
#endif
T(cf)
#endif

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
