#include "compat-common.h"

#define T(N, TYPE)						\
struct S##TYPE##N { TYPE i[N]; };				\
								\
struct S##TYPE##N g1s##TYPE##N, g2s##TYPE##N;			\
struct S##TYPE##N g3s##TYPE##N, g4s##TYPE##N;			\
struct S##TYPE##N g5s##TYPE##N, g6s##TYPE##N;			\
struct S##TYPE##N g7s##TYPE##N, g8s##TYPE##N;			\
struct S##TYPE##N g9s##TYPE##N, g10s##TYPE##N;			\
struct S##TYPE##N g11s##TYPE##N, g12s##TYPE##N;			\
struct S##TYPE##N g13s##TYPE##N, g14s##TYPE##N;			\
struct S##TYPE##N g15s##TYPE##N, g16s##TYPE##N;			\
								\
extern void init##TYPE (TYPE *p, int i);			\
extern void checkg##TYPE##N (void);				\
extern void							\
test##TYPE##N (struct S##TYPE##N s1, struct S##TYPE##N s2,	\
	       struct S##TYPE##N s3, struct S##TYPE##N s4,	\
	       struct S##TYPE##N s5, struct S##TYPE##N s6,	\
	       struct S##TYPE##N s7, struct S##TYPE##N s8,	\
	       struct S##TYPE##N s9, struct S##TYPE##N s10,	\
	       struct S##TYPE##N s11, struct S##TYPE##N s12,	\
	       struct S##TYPE##N s13, struct S##TYPE##N s14,	\
	       struct S##TYPE##N s15, struct S##TYPE##N s16);	\
extern void testva##TYPE##N (int n, ...);			\
								\
								\
void								\
init##TYPE##N (struct S##TYPE##N *p, int i)			\
{								\
  int j;							\
  for (j = 0; j < N; j++)					\
    init##TYPE(&p->i[j], i+j);					\
}								\
								\
void								\
check##TYPE##N (struct S##TYPE##N *p, int i)			\
{								\
  int j;							\
  for (j = 0; j < N; j++)					\
    check##TYPE(p->i[j], i+j);					\
}								\
								\
void								\
test2_##TYPE##N (struct S##TYPE##N s1, struct S##TYPE##N s2,	\
		 struct S##TYPE##N s3, struct S##TYPE##N s4,	\
		 struct S##TYPE##N s5, struct S##TYPE##N s6,	\
		 struct S##TYPE##N s7, struct S##TYPE##N s8)	\
{								\
  test##TYPE##N (s1, g2s##TYPE##N, s2, g4s##TYPE##N,		\
		 s3, g6s##TYPE##N, s4, g8s##TYPE##N,		\
		 s5, g10s##TYPE##N, s6, g12s##TYPE##N,		\
		 s7, g14s##TYPE##N, s8, g16s##TYPE##N);		\
}								\
								\
void								\
testit##TYPE##N (void)						\
{								\
  DEBUG_FPUTS (#TYPE "[" #N "]");				\
  DEBUG_FPUTS (" init: ");					\
  init##TYPE##N  ( &g1s##TYPE##N,  1*16);			\
  init##TYPE##N  ( &g2s##TYPE##N,  2*16);			\
  init##TYPE##N  ( &g3s##TYPE##N,  3*16);			\
  init##TYPE##N  ( &g4s##TYPE##N,  4*16);			\
  init##TYPE##N  ( &g5s##TYPE##N,  5*16);			\
  init##TYPE##N  ( &g6s##TYPE##N,  6*16);			\
  init##TYPE##N  ( &g7s##TYPE##N,  7*16);			\
  init##TYPE##N  ( &g8s##TYPE##N,  8*16);			\
  init##TYPE##N  ( &g9s##TYPE##N,  9*16);			\
  init##TYPE##N  (&g10s##TYPE##N, 10*16);			\
  init##TYPE##N  (&g11s##TYPE##N, 11*16);			\
  init##TYPE##N  (&g12s##TYPE##N, 12*16);			\
  init##TYPE##N  (&g13s##TYPE##N, 13*16);			\
  init##TYPE##N  (&g14s##TYPE##N, 14*16);			\
  init##TYPE##N  (&g15s##TYPE##N, 15*16);			\
  init##TYPE##N  (&g16s##TYPE##N, 16*16);			\
  checkg##TYPE##N ();						\
  DEBUG_NL;							\
  DEBUG_FPUTS (#TYPE "[" #N "]");				\
  DEBUG_FPUTS (" test: ");					\
  test##TYPE##N (g1s##TYPE##N, g2s##TYPE##N,			\
		 g3s##TYPE##N, g4s##TYPE##N,			\
		 g5s##TYPE##N, g6s##TYPE##N,			\
		 g7s##TYPE##N, g8s##TYPE##N,			\
		 g9s##TYPE##N, g10s##TYPE##N,			\
		 g11s##TYPE##N, g12s##TYPE##N,			\
		 g13s##TYPE##N, g14s##TYPE##N,			\
		 g15s##TYPE##N, g16s##TYPE##N);			\
  DEBUG_NL;							\
  DEBUG_FPUTS (#TYPE "[" #N "]");				\
  DEBUG_FPUTS (" testva:");					\
  testva##TYPE##N (16,						\
		   g1s##TYPE##N, g2s##TYPE##N,			\
		   g3s##TYPE##N, g4s##TYPE##N,			\
		   g5s##TYPE##N, g6s##TYPE##N,			\
		   g7s##TYPE##N, g8s##TYPE##N,			\
		   g9s##TYPE##N, g10s##TYPE##N,			\
		   g11s##TYPE##N, g12s##TYPE##N,		\
		   g13s##TYPE##N, g14s##TYPE##N,		\
		   g15s##TYPE##N, g16s##TYPE##N);		\
  DEBUG_NL;							\
  DEBUG_FPUTS (#TYPE "[" #N "]");				\
  DEBUG_FPUTS (" test2: ");					\
  test2_##TYPE##N (g1s##TYPE##N, g3s##TYPE##N,			\
		   g5s##TYPE##N, g7s##TYPE##N,			\
		   g9s##TYPE##N, g11s##TYPE##N,			\
		   g13s##TYPE##N, g15s##TYPE##N);		\
  DEBUG_NL;							\
}

typedef struct { char c; } Sc;
typedef struct { short s; } Ss;
typedef struct { int i; } Si;
typedef struct { short s; char c; } Ssc;
typedef struct { int i; short s; } Sis;
typedef struct { char c; short s; int i; } Scsi;
typedef struct { char c; int i; short s; } Scis;

void checkSc (Sc x, int i) { if (x.c != i/16) DEBUG_CHECK }
void checkSs (Ss x, int i) { if (x.s != i) DEBUG_CHECK }
void checkSi (Si x, int i) { if (x.i != i) DEBUG_CHECK }
void checkSsc (Ssc x, int i)
{ if (x.s != i || x.c != (i/16)+1) DEBUG_CHECK }
void checkSis (Sis x, int i)
{ if (x.i != i || x.s != i+1) DEBUG_CHECK }
void checkScsi (Scsi x, int i)
{ if (x.c != i/16 || x.s != i+1 || x.i != i+2) DEBUG_CHECK }
void checkScis (Scis x, int i)
{ if (x.c != i/16 || x.i != i+1 || x.s != i+2) DEBUG_CHECK }

#ifndef SKIP_ZERO_ARRAY
T(0, Ssc)
#endif
T(1, Ssc)
T(2, Ssc)
T(3, Ssc)
T(4, Ssc)
T(5, Ssc)
T(6, Ssc)
T(7, Ssc)
T(8, Ssc)
T(9, Ssc)
T(10, Ssc)
T(11, Ssc)
T(12, Ssc)
T(13, Ssc)
T(14, Ssc)
T(15, Ssc)
#ifndef SKIP_ZERO_ARRAY
T(0, Sis)
#endif
T(1, Sis)
T(2, Sis)
T(3, Sis)
T(4, Sis)
T(5, Sis)
T(6, Sis)
T(7, Sis)
T(8, Sis)
T(9, Sis)
T(10, Sis)
T(11, Sis)
T(12, Sis)
T(13, Sis)
T(14, Sis)
T(15, Sis)

#undef T

void
struct_by_value_8_x ()
{
DEBUG_INIT

#define T(N, TYPE) testit##TYPE##N ();

#ifndef SKIP_ZERO_ARRAY
T(0, Ssc)
#endif
T(1, Ssc)
T(2, Ssc)
T(3, Ssc)
T(4, Ssc)
T(5, Ssc)
T(6, Ssc)
T(7, Ssc)
T(8, Ssc)
T(9, Ssc)
T(10, Ssc)
T(11, Ssc)
T(12, Ssc)
T(13, Ssc)
T(14, Ssc)
T(15, Ssc)
#ifndef SKIP_ZERO_ARRAY
T(0, Sis)
#endif
T(1, Sis)
T(2, Sis)
T(3, Sis)
T(4, Sis)
T(5, Sis)
T(6, Sis)
T(7, Sis)
T(8, Sis)
T(9, Sis)
T(10, Sis)
T(11, Sis)
T(12, Sis)
T(13, Sis)
T(14, Sis)
T(15, Sis)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
