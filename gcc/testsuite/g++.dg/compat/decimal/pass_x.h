#include "compat-common.h"

#define T(NAME, TYPE, INITVAL)					\
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
extern void testva##NAME (int n, ...);				\
								\
extern void							\
check##NAME (TYPE x, TYPE v)					\
{								\
  if (x != v + INITVAL)						\
    DEBUG_CHECK							\
}								\
								\
extern void							\
test2_##NAME (TYPE x01, TYPE x02, TYPE x03, TYPE x04,		\
	      TYPE x05, TYPE x06, TYPE x07, TYPE x08)		\
{								\
  test##NAME (x01, g02##NAME, x02, g04##NAME,			\
	      x03, g06##NAME, x04, g08##NAME,			\
	      x05, g10##NAME, x06, g12##NAME,			\
	      x07, g14##NAME, x08, g16##NAME);			\
}								\
								\
extern void							\
testit##NAME (void)						\
{								\
  DEBUG_FPUTS (#NAME)						\
  DEBUG_FPUTS (" init: ")					\
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
  DEBUG_NL							\
  DEBUG_FPUTS (#NAME)						\
  DEBUG_FPUTS (" test: ")					\
  test##NAME (g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
	      g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
	      g09##NAME, g10##NAME, g11##NAME, g12##NAME,	\
	      g13##NAME, g14##NAME, g15##NAME, g16##NAME);	\
  DEBUG_NL							\
  DEBUG_FPUTS (#NAME)						\
  DEBUG_FPUTS (" testva: ")					\
  DEBUG_NL							\
  testva##NAME (1,						\
		g01##NAME);					\
  DEBUG_NL							\
  testva##NAME (2,						\
		g01##NAME, g02##NAME);				\
  DEBUG_NL							\
  testva##NAME (3,						\
		g01##NAME, g02##NAME, g03##NAME);		\
  DEBUG_NL							\
  testva##NAME (4,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME);	\
  DEBUG_NL							\
  testva##NAME (5,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME);					\
  DEBUG_NL							\
  testva##NAME (6,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME);				\
  DEBUG_NL							\
  testva##NAME (7,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME);		\
  DEBUG_NL							\
  testva##NAME (8,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME);	\
  DEBUG_NL							\
  testva##NAME (9,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME);					\
  DEBUG_NL							\
  testva##NAME (10,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME);				\
  DEBUG_NL							\
  testva##NAME (11,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME, g11##NAME);		\
  DEBUG_NL							\
  testva##NAME (12,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME, g11##NAME, g12##NAME);	\
  DEBUG_NL							\
  testva##NAME (13,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME, g11##NAME, g12##NAME,	\
		g13##NAME);					\
  DEBUG_NL							\
  testva##NAME (14,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME, g11##NAME, g12##NAME,	\
		g13##NAME, g14##NAME);				\
  DEBUG_NL							\
  testva##NAME (15,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME, g11##NAME, g12##NAME,	\
		g13##NAME, g14##NAME, g15##NAME);		\
  DEBUG_NL							\
  testva##NAME (16,						\
		g01##NAME, g02##NAME, g03##NAME, g04##NAME,	\
		g05##NAME, g06##NAME, g07##NAME, g08##NAME,	\
		g09##NAME, g10##NAME, g11##NAME, g12##NAME,	\
		g13##NAME, g14##NAME, g15##NAME, g16##NAME);	\
  DEBUG_NL							\
  DEBUG_FPUTS (#NAME)						\
  DEBUG_FPUTS (" test2: ")					\
  test2_##NAME (g01##NAME, g03##NAME, g05##NAME, g07##NAME,	\
		g09##NAME, g11##NAME, g13##NAME, g15##NAME);	\
  DEBUG_NL							\
}

T(d32, dec32, (dec32)1.5DF)
T(d64, dec64, (dec64)2.5DD)
T(d128, dec128, (dec128)3.5DL)

#undef T
