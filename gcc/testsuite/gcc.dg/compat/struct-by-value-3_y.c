#include <stdarg.h>

#ifdef DBG
#include <stdio.h>
#define DEBUG_FPUTS(x) fputs (x, stdout)
#define DEBUG_DOT putc ('.', stdout)
#define DEBUG_NL putc ('\n', stdout)
#else
#define DEBUG_FPUTS(x)
#define DEBUG_DOT
#define DEBUG_NL
#endif

/* Turn off checking for variable arguments with -DSKIPVA.  */
#ifdef SKIPVA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "small-struct-defs.h"

extern void checkSc (Sc x, int i);
extern void checkSs (Ss x, int i);
extern void checkSi (Si x, int i);
extern void checkSsc (Ssc x, int i);
extern void checkScs (Scs x, int i);
extern void checkSsi (Ssi x, int i);
extern void checkSis (Sis x, int i);
extern void checkSic (Sic x, int i);
extern void checkSci (Sci x, int i);
extern void checkScsi (Scsi x, int i);
extern void checkScis (Scis x, int i);
extern void checkSsci (Ssci x, int i);
extern void checkSsic (Ssic x, int i);
extern void checkSisc (Sisc x, int i);
extern void checkSics (Sics x, int i);

#include "small-struct-init.h"

#define T(TYPE)							\
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
  DEBUG_DOT;							\
  check##TYPE (s1, 1);						\
  DEBUG_DOT;							\
  check##TYPE (s2, 2);						\
  DEBUG_DOT;							\
  check##TYPE (s3, 3);						\
  DEBUG_DOT;							\
  check##TYPE (s4, 4);						\
  DEBUG_DOT;							\
  check##TYPE (s5, 5);						\
  DEBUG_DOT;							\
  check##TYPE (s6, 6);						\
  DEBUG_DOT;							\
  check##TYPE (s7, 7);						\
  DEBUG_DOT;							\
  check##TYPE (s8, 8);						\
  DEBUG_DOT;							\
  check##TYPE (s9, 9);						\
  DEBUG_DOT;							\
  check##TYPE (s10, 10);					\
  DEBUG_DOT;							\
  check##TYPE (s11, 11);					\
  DEBUG_DOT;							\
  check##TYPE (s12, 12);					\
  DEBUG_DOT;							\
  check##TYPE (s13, 13);					\
  DEBUG_DOT;							\
  check##TYPE (s14, 14);					\
  DEBUG_DOT;							\
  check##TYPE (s15, 15);					\
  DEBUG_DOT;							\
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
	  DEBUG_DOT;						\
	  check##TYPE (t, i+1);					\
	}							\
      va_end (ap);						\
    }								\
}

T(Sc)
T(Ss)
T(Si)
T(Ssc)
T(Scs)
T(Sic)
T(Sci)
T(Ssi)
T(Sis)
T(Scsi)
T(Scis)
T(Ssci)
T(Ssic)
T(Sisc)
T(Sics)
