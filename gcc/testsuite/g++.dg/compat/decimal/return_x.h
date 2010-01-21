#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#define T(NAME, TYPE, INITVAL) 					\
TYPE g01##NAME, g02##NAME, g03##NAME, g04##NAME;		\
TYPE g05##NAME, g06##NAME, g07##NAME, g08##NAME;		\
TYPE g09##NAME, g10##NAME, g11##NAME, g12##NAME;		\
TYPE g13##NAME, g14##NAME, g15##NAME, g16##NAME;		\
								\
extern void init##NAME (TYPE *p, TYPE v);			\
extern void checkg##NAME (void);				\
extern TYPE test0##NAME (void);					\
extern TYPE test1##NAME (TYPE);					\
extern TYPE testva##NAME (int n, ...);				\
								\
extern void							\
check##NAME (TYPE x, TYPE v)					\
{								\
  if (x != v)							\
    DEBUG_CHECK							\
}								\
								\
extern void							\
testit##NAME (void)						\
{								\
  TYPE rslt;							\
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
  DEBUG_FPUTS (" test0: ")					\
  rslt = test0##NAME ();					\
  check##NAME (rslt, g01##NAME);				\
  DEBUG_NL							\
  DEBUG_FPUTS (#NAME)						\
  DEBUG_FPUTS (" test1: ")					\
  rslt = test1##NAME (g01##NAME);				\
  check##NAME (rslt, g01##NAME);				\
  if (test_va)							\
    {								\
      DEBUG_NL							\
      DEBUG_FPUTS (#NAME)					\
      DEBUG_FPUTS (" testva: ")					\
      rslt = testva##NAME (1, g01##NAME);			\
      check##NAME (rslt, g01##NAME);				\
      rslt = testva##NAME (5, g01##NAME, g02##NAME, g03##NAME,	\
			   g04##NAME, g05##NAME);		\
      check##NAME (rslt, g05##NAME);				\
      rslt = testva##NAME (9, g01##NAME, g02##NAME, g03##NAME,	\
			   g04##NAME, g05##NAME, g06##NAME,	\
			   g07##NAME, g08##NAME, g09##NAME);	\
      check##NAME (rslt, g09##NAME);				\
      rslt = testva##NAME (16, g01##NAME, g02##NAME, g03##NAME,	\
			   g04##NAME, g05##NAME, g06##NAME,	\
			   g07##NAME, g08##NAME, g09##NAME,	\
			   g10##NAME, g11##NAME, g12##NAME,	\
			   g13##NAME, g14##NAME, g15##NAME,	\
			   g16##NAME);				\
      check##NAME (rslt, g16##NAME);				\
    }								\
  DEBUG_NL							\
}

T(d32, dec32, (dec32)1.5DF);
T(d64, dec64, (dec64)2.5DD);
T(d128, dec128, (dec128)3.5DL);

#undef T
