#include "compat-common.h"

#define T(N, NAME, TYPE)					\
struct S##NAME##N { TYPE i[N]; };				\
struct S##NAME##N g1s##NAME##N, g2s##NAME##N;			\
struct S##NAME##N g3s##NAME##N, g4s##NAME##N;			\
struct S##NAME##N g5s##NAME##N, g6s##NAME##N;			\
struct S##NAME##N g7s##NAME##N, g8s##NAME##N;			\
struct S##NAME##N g9s##NAME##N, g10s##NAME##N;			\
struct S##NAME##N g11s##NAME##N, g12s##NAME##N;			\
struct S##NAME##N g13s##NAME##N, g14s##NAME##N;			\
struct S##NAME##N g15s##NAME##N, g16s##NAME##N;			\
								\
extern void init##NAME##N (struct S##NAME##N *p, int i);	\
extern void checkg##NAME##N (void);				\
extern void							\
test##NAME##N (struct S##NAME##N s1, struct S##NAME##N s2,	\
	       struct S##NAME##N s3, struct S##NAME##N s4,	\
	       struct S##NAME##N s5, struct S##NAME##N s6,	\
	       struct S##NAME##N s7, struct S##NAME##N s8,	\
	       struct S##NAME##N s9, struct S##NAME##N s10,	\
	       struct S##NAME##N s11, struct S##NAME##N s12,	\
	       struct S##NAME##N s13, struct S##NAME##N s14,	\
	       struct S##NAME##N s15, struct S##NAME##N s16);	\
extern void testva##NAME##N (int n, ...);			\
extern void							\
test2_##NAME##N (struct S##NAME##N s1, struct S##NAME##N s2,	\
		 struct S##NAME##N s3, struct S##NAME##N s4,	\
		 struct S##NAME##N s5, struct S##NAME##N s6,	\
		 struct S##NAME##N s7, struct S##NAME##N s8);	\
								\
void								\
check##NAME##N (struct S##NAME##N *p, int i)			\
{								\
  int j;							\
  for (j = 0; j < N; j++)					\
    if (p->i[j] != (TYPE) (i + j))				\
      DEBUG_CHECK						\
}								\
								\
void								\
test2_##NAME##N (struct S##NAME##N s1, struct S##NAME##N s2,	\
		 struct S##NAME##N s3, struct S##NAME##N s4,	\
		 struct S##NAME##N s5, struct S##NAME##N s6,	\
		 struct S##NAME##N s7, struct S##NAME##N s8)	\
{								\
  test##NAME##N (s1, g2s##NAME##N, s2, g4s##NAME##N,		\
		 s3, g6s##NAME##N, s4, g8s##NAME##N,		\
		 s5, g10s##NAME##N, s6, g12s##NAME##N,		\
		 s7, g14s##NAME##N, s8, g16s##NAME##N);		\
}								\
								\
void								\
testit##NAME##N (void)						\
{								\
  DEBUG_FPUTS (#NAME "[" #N "]");				\
  DEBUG_FPUTS (" init: ");					\
  init##NAME##N  ( &g1s##NAME##N,  1*16);			\
  init##NAME##N  ( &g2s##NAME##N,  2*16);			\
  init##NAME##N  ( &g3s##NAME##N,  3*16);			\
  init##NAME##N  ( &g4s##NAME##N,  4*16);			\
  init##NAME##N  ( &g5s##NAME##N,  5*16);			\
  init##NAME##N  ( &g6s##NAME##N,  6*16);			\
  init##NAME##N  ( &g7s##NAME##N,  7*16);			\
  init##NAME##N  ( &g8s##NAME##N,  8*16);			\
  init##NAME##N  ( &g9s##NAME##N,  9*16);			\
  init##NAME##N  (&g10s##NAME##N, 10*16);			\
  init##NAME##N  (&g11s##NAME##N, 11*16);			\
  init##NAME##N  (&g12s##NAME##N, 12*16);			\
  init##NAME##N  (&g13s##NAME##N, 13*16);			\
  init##NAME##N  (&g14s##NAME##N, 14*16);			\
  init##NAME##N  (&g15s##NAME##N, 15*16);			\
  init##NAME##N  (&g16s##NAME##N, 16*16);			\
  checkg##NAME##N ();						\
  DEBUG_NL;							\
  DEBUG_FPUTS (#NAME "[" #N "]");				\
  DEBUG_FPUTS (" test: ");					\
  test##NAME##N (g1s##NAME##N, g2s##NAME##N,			\
		 g3s##NAME##N, g4s##NAME##N,			\
		 g5s##NAME##N, g6s##NAME##N,			\
		 g7s##NAME##N, g8s##NAME##N,			\
		 g9s##NAME##N, g10s##NAME##N,			\
		 g11s##NAME##N, g12s##NAME##N,			\
		 g13s##NAME##N, g14s##NAME##N,			\
		 g15s##NAME##N, g16s##NAME##N);			\
  DEBUG_NL;							\
  DEBUG_FPUTS (#NAME "[" #N "]");				\
  DEBUG_FPUTS (" testva:");					\
  testva##NAME##N (16,						\
		   g1s##NAME##N, g2s##NAME##N,			\
		   g3s##NAME##N, g4s##NAME##N,			\
		   g5s##NAME##N, g6s##NAME##N,			\
		   g7s##NAME##N, g8s##NAME##N,			\
		   g9s##NAME##N, g10s##NAME##N,			\
		   g11s##NAME##N, g12s##NAME##N,		\
		   g13s##NAME##N, g14s##NAME##N,		\
		   g15s##NAME##N, g16s##NAME##N);		\
  DEBUG_NL;							\
  DEBUG_FPUTS (#NAME "[" #N "]");				\
  DEBUG_FPUTS (" test2: ");					\
  test2_##NAME##N (g1s##NAME##N, g3s##NAME##N,			\
		   g5s##NAME##N, g7s##NAME##N,			\
		   g9s##NAME##N, g11s##NAME##N,			\
		   g13s##NAME##N, g15s##NAME##N);		\
  DEBUG_NL;							\
}

#ifndef SKIP_ZERO_ARRAY
T(0, uc, unsigned char)
#endif
T(1, uc, unsigned char)
T(2, uc, unsigned char)
T(3, uc, unsigned char)
T(4, uc, unsigned char)
T(5, uc, unsigned char)
T(6, uc, unsigned char)
T(7, uc, unsigned char)
T(8, uc, unsigned char)
T(9, uc, unsigned char)
T(10, uc, unsigned char)
T(11, uc, unsigned char)
T(12, uc, unsigned char)
T(13, uc, unsigned char)
T(14, uc, unsigned char)
T(15, uc, unsigned char)
#ifndef SKIP_ZERO_ARRAY
T(0, us, unsigned short)
#endif
T(1, us, unsigned short)
T(2, us, unsigned short)
T(3, us, unsigned short)
T(4, us, unsigned short)
T(5, us, unsigned short)
T(6, us, unsigned short)
T(7, us, unsigned short)
T(8, us, unsigned short)
T(9, us, unsigned short)
T(10, us, unsigned short)
T(11, us, unsigned short)
T(12, us, unsigned short)
T(13, us, unsigned short)
T(14, us, unsigned short)
T(15, us, unsigned short)
#ifndef SKIP_ZERO_ARRAY
T(0, ui, unsigned int)
#endif
T(1, ui, unsigned int)
T(2, ui, unsigned int)
T(3, ui, unsigned int)
T(4, ui, unsigned int)
T(5, ui, unsigned int)
T(6, ui, unsigned int)
T(7, ui, unsigned int)
T(8, ui, unsigned int)
T(9, ui, unsigned int)
T(10, ui, unsigned int)
T(11, ui, unsigned int)
T(12, ui, unsigned int)
T(13, ui, unsigned int)
T(14, ui, unsigned int)
T(15, ui, unsigned int)

#undef T

void
struct_by_value_2_x ()
{
DEBUG_INIT

#define T(N, NAME, TYPE) testit##NAME##N ();

#ifndef SKIP_ZERO_ARRAY
T(0, uc, unsigned char)
#endif
T(1, uc, unsigned char)
T(2, uc, unsigned char)
T(3, uc, unsigned char)
T(4, uc, unsigned char)
T(5, uc, unsigned char)
T(6, uc, unsigned char)
T(7, uc, unsigned char)
T(8, uc, unsigned char)
T(9, uc, unsigned char)
T(10, uc, unsigned char)
T(11, uc, unsigned char)
T(12, uc, unsigned char)
T(13, uc, unsigned char)
T(14, uc, unsigned char)
T(15, uc, unsigned char)
#ifndef SKIP_ZERO_ARRAY
T(0, us, unsigned short)
#endif
T(1, us, unsigned short)
T(2, us, unsigned short)
T(3, us, unsigned short)
T(4, us, unsigned short)
T(5, us, unsigned short)
T(6, us, unsigned short)
T(7, us, unsigned short)
T(8, us, unsigned short)
T(9, us, unsigned short)
T(10, us, unsigned short)
T(11, us, unsigned short)
T(12, us, unsigned short)
T(13, us, unsigned short)
T(14, us, unsigned short)
T(15, us, unsigned short)
#ifndef SKIP_ZERO_ARRAY
T(0, ui, unsigned int)
#endif
T(1, ui, unsigned int)
T(2, ui, unsigned int)
T(3, ui, unsigned int)
T(4, ui, unsigned int)
T(5, ui, unsigned int)
T(6, ui, unsigned int)
T(7, ui, unsigned int)
T(8, ui, unsigned int)
T(9, ui, unsigned int)
T(10, ui, unsigned int)
T(11, ui, unsigned int)
T(12, ui, unsigned int)
T(13, ui, unsigned int)
T(14, ui, unsigned int)
T(15, ui, unsigned int)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
