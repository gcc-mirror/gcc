#include <stdarg.h>

#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

typedef struct { char c; } Sc;
typedef struct { short s; } Ss;
typedef struct { int i; } Si;
typedef struct { short s; char c; } Ssc;
typedef struct { int i; short s; } Sis;
typedef struct { char c; short s; int i; } Scsi;
typedef struct { char c; int i; short s; } Scis;

void initSc (Sc *p, int i) { p->c = i/16; }
void initSs (Ss *p, int i) { p->s = i; }
void initSi (Si *p, int i) { p->i = i; }
void initSsc (Ssc *p, int i) { p->s = i; p->c = (i/16)+1; }
void initSis (Sis *p, int i) { p->i = i; p->s = i+1; }
void initScsi (Scsi *p, int i) { p->c = i/16; p->s = i+1; p->i = i+2; }
void initScis (Scis *p, int i) { p->c = i/16; p->i = i+1; p->s = i+2; }

#define T(N, TYPE)						\
struct S##TYPE##N { TYPE i[N]; };				\
								\
extern struct S##TYPE##N g1s##TYPE##N, g2s##TYPE##N;		\
extern struct S##TYPE##N g3s##TYPE##N, g4s##TYPE##N;		\
extern struct S##TYPE##N g5s##TYPE##N, g6s##TYPE##N;		\
extern struct S##TYPE##N g7s##TYPE##N, g8s##TYPE##N;		\
extern struct S##TYPE##N g9s##TYPE##N, g10s##TYPE##N;		\
extern struct S##TYPE##N g11s##TYPE##N, g12s##TYPE##N;		\
extern struct S##TYPE##N g13s##TYPE##N, g14s##TYPE##N;		\
extern struct S##TYPE##N g15s##TYPE##N, g16s##TYPE##N;		\
								\
extern void check##TYPE (TYPE x, int i);			\
extern void							\
check##TYPE##N (struct S##TYPE##N *p, int i);			\
								\
void								\
checkg##TYPE##N (void)						\
{								\
  check##TYPE##N ( &g1s##TYPE##N,  1*16);			\
  check##TYPE##N ( &g2s##TYPE##N,  2*16);			\
  check##TYPE##N ( &g3s##TYPE##N,  3*16);			\
  check##TYPE##N ( &g4s##TYPE##N,  4*16);			\
  check##TYPE##N ( &g5s##TYPE##N,  5*16);			\
  check##TYPE##N ( &g6s##TYPE##N,  6*16);			\
  check##TYPE##N ( &g7s##TYPE##N,  7*16);			\
  check##TYPE##N ( &g8s##TYPE##N,  8*16);			\
  check##TYPE##N ( &g9s##TYPE##N,  9*16);			\
  check##TYPE##N (&g10s##TYPE##N, 10*16);			\
  check##TYPE##N (&g11s##TYPE##N, 11*16);			\
  check##TYPE##N (&g12s##TYPE##N, 12*16);			\
  check##TYPE##N (&g13s##TYPE##N, 13*16);			\
  check##TYPE##N (&g14s##TYPE##N, 14*16);			\
  check##TYPE##N (&g15s##TYPE##N, 15*16);			\
  check##TYPE##N (&g16s##TYPE##N, 16*16);			\
}								\
								\
void								\
test##TYPE##N (struct S##TYPE##N s1, struct S##TYPE##N s2,	\
	       struct S##TYPE##N s3, struct S##TYPE##N s4,	\
	       struct S##TYPE##N s5, struct S##TYPE##N s6,	\
	       struct S##TYPE##N s7, struct S##TYPE##N s8,	\
	       struct S##TYPE##N s9, struct S##TYPE##N s10,	\
	       struct S##TYPE##N s11, struct S##TYPE##N s12,	\
	       struct S##TYPE##N s13, struct S##TYPE##N s14,	\
	       struct S##TYPE##N s15, struct S##TYPE##N s16)	\
{								\
  check##TYPE##N (&s1, 1*16);					\
  check##TYPE##N (&s2, 2*16);					\
  check##TYPE##N (&s3, 3*16);					\
  check##TYPE##N (&s4, 4*16);					\
  check##TYPE##N (&s5, 5*16);					\
  check##TYPE##N (&s6, 6*16);					\
  check##TYPE##N (&s7, 7*16);					\
  check##TYPE##N (&s8, 8*16);					\
  check##TYPE##N (&s9, 9*16);					\
  check##TYPE##N (&s10, 10*16);					\
  check##TYPE##N (&s11, 11*16);					\
  check##TYPE##N (&s12, 12*16);					\
  check##TYPE##N (&s13, 13*16);					\
  check##TYPE##N (&s14, 14*16);					\
  check##TYPE##N (&s15, 15*16);					\
  check##TYPE##N (&s16, 16*16);					\
}								\
								\
void								\
testva##TYPE##N (int n, ...)					\
{								\
  int i;							\
  va_list ap;							\
  if (test_va)							\
    {								\
      va_start (ap, n);						\
      for (i = 0; i < n; i++)					\
	{							\
	  struct S##TYPE##N t = va_arg (ap, struct S##TYPE##N);	\
	  check##TYPE##N (&t, (i+1)*16);			\
	}							\
      va_end (ap);						\
    }								\
}

#ifndef SKIP_ZERO_ARRAY
T(0, Scsi)
#endif
T(1, Scsi)
T(2, Scsi)
T(3, Scsi)
T(4, Scsi)
T(5, Scsi)
T(6, Scsi)
T(7, Scsi)
T(8, Scsi)
T(9, Scsi)
T(10, Scsi)
T(11, Scsi)
T(12, Scsi)
T(13, Scsi)
T(14, Scsi)
T(15, Scsi)
#ifndef SKIP_ZERO_ARRAY
T(0, Scis)
#endif
T(1, Scis)
T(2, Scis)
T(3, Scis)
T(4, Scis)
T(5, Scis)
T(6, Scis)
T(7, Scis)
T(8, Scis)
T(9, Scis)
T(10, Scis)
T(11, Scis)
T(12, Scis)
T(13, Scis)
T(14, Scis)
T(15, Scis)
