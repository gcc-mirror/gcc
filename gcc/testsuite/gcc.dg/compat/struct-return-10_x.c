#include "compat-common.h"

#define T(TYPE)							\
TYPE g01##TYPE, g02##TYPE, g03##TYPE, g04##TYPE;		\
TYPE g05##TYPE, g06##TYPE, g07##TYPE, g08##TYPE;		\
TYPE g09##TYPE, g10##TYPE, g11##TYPE, g12##TYPE;		\
TYPE g13##TYPE, g14##TYPE, g15##TYPE, g16##TYPE;		\
								\
extern void init##TYPE (TYPE *p, double y);			\
extern void checkg##TYPE (void);				\
extern TYPE test0##TYPE (void);					\
extern TYPE test1##TYPE (TYPE);					\
extern TYPE testva##TYPE (int n, ...);				\
								\
void								\
testit##TYPE (void)						\
{								\
  TYPE rslt;							\
  DEBUG_FPUTS (#TYPE);						\
  DEBUG_FPUTS (" init: ");					\
  init##TYPE  (&g01##TYPE,  1.0);				\
  init##TYPE  (&g02##TYPE,  2.0);				\
  init##TYPE  (&g03##TYPE,  3.0);				\
  init##TYPE  (&g04##TYPE,  4.0);				\
  init##TYPE  (&g05##TYPE,  5.0);				\
  init##TYPE  (&g06##TYPE,  6.0);				\
  init##TYPE  (&g07##TYPE,  7.0);				\
  init##TYPE  (&g08##TYPE,  8.0);				\
  init##TYPE  (&g09##TYPE,  9.0);				\
  init##TYPE  (&g10##TYPE, 10.0);				\
  init##TYPE  (&g11##TYPE, 11.0);				\
  init##TYPE  (&g12##TYPE, 12.0);				\
  init##TYPE  (&g13##TYPE, 13.0);				\
  init##TYPE  (&g14##TYPE, 14.0);				\
  init##TYPE  (&g15##TYPE, 15.0);				\
  init##TYPE  (&g16##TYPE, 16.0);				\
  checkg##TYPE ();						\
  DEBUG_NL;							\
  DEBUG_FPUTS (#TYPE);						\
  DEBUG_FPUTS (" test0: ");					\
  rslt = test0##TYPE ();					\
  check##TYPE (rslt, 1.0);					\
  DEBUG_NL;							\
  DEBUG_FPUTS (#TYPE);						\
  DEBUG_FPUTS (" test1: ");					\
  rslt = test1##TYPE (g01##TYPE);				\
  check##TYPE (rslt, 1.0);					\
  DEBUG_NL;							\
  DEBUG_FPUTS (#TYPE);						\
  DEBUG_FPUTS (" testva:");					\
  rslt = testva##TYPE (1, g01##TYPE);				\
  check##TYPE (rslt, 1.0);					\
  rslt = testva##TYPE (5, g01##TYPE, g02##TYPE,			\
			  g03##TYPE, g04##TYPE,			\
			  g05##TYPE);				\
  check##TYPE (rslt, 5.0);					\
  rslt = testva##TYPE (9, g01##TYPE, g02##TYPE,			\
			  g03##TYPE, g04##TYPE,			\
			  g05##TYPE, g06##TYPE,			\
			  g07##TYPE, g08##TYPE,			\
			  g09##TYPE);				\
  check##TYPE (rslt, 9.0);					\
  rslt = testva##TYPE (16, g01##TYPE, g02##TYPE,		\
			  g03##TYPE, g04##TYPE,			\
			  g05##TYPE, g06##TYPE,			\
			  g07##TYPE, g08##TYPE,			\
			  g09##TYPE, g10##TYPE,			\
			  g11##TYPE, g12##TYPE,			\
			  g13##TYPE, g14##TYPE,			\
			  g15##TYPE, g16##TYPE);		\
  check##TYPE (rslt, 16.0);					\
  DEBUG_NL;							\
}

#include "fp2-struct-defs.h"
#include "fp2-struct-check.h"

T(Sfd)
T(Sfl)
T(Sdf)
T(Sdl)
T(Slf)
T(Sld)
T(Sfdl)
T(Sfld)
T(Sdfl)
T(Sdlf)
T(Slfd)
T(Sldf)

#undef T

void
struct_return_10_x ()
{
DEBUG_INIT

#define T(TYPE) testit##TYPE ();

T(Sfd);
T(Sfl);
T(Sdf);
T(Sdl);
T(Slf);
T(Sld);
T(Sfdl);
T(Sfld);
T(Sdfl);
T(Sdlf);
T(Slfd);
T(Sldf);

DEBUG_FINI

if (fails != 0)
  return;

#undef T
}
