#include <stdarg.h>

#include "compat-common.h"

#define T(N, NAME, TYPE)					\
struct S##NAME##N { TYPE i[N]; };				\
								\
extern struct S##NAME##N g1s##NAME##N, g2s##NAME##N;		\
extern struct S##NAME##N g3s##NAME##N, g4s##NAME##N;		\
extern struct S##NAME##N g5s##NAME##N, g6s##NAME##N;		\
extern struct S##NAME##N g7s##NAME##N, g8s##NAME##N;		\
extern struct S##NAME##N g9s##NAME##N, g10s##NAME##N;		\
extern struct S##NAME##N g11s##NAME##N, g12s##NAME##N;		\
extern struct S##NAME##N g13s##NAME##N, g14s##NAME##N;		\
extern struct S##NAME##N g15s##NAME##N, g16s##NAME##N;		\
								\
extern void check##NAME##N (struct S##NAME##N *p, int i);	\
								\
void								\
init##NAME##N (struct S##NAME##N *p, int i)			\
{								\
  int j;							\
  for (j = 0; j < N; j++)					\
    p->i[j] = i + j;						\
}								\
								\
void								\
checkg##NAME##N (void)						\
{								\
  check##NAME##N ( &g1s##NAME##N,  1*16);			\
  check##NAME##N ( &g2s##NAME##N,  2*16);			\
  check##NAME##N ( &g3s##NAME##N,  3*16);			\
  check##NAME##N ( &g4s##NAME##N,  4*16);			\
  check##NAME##N ( &g5s##NAME##N,  5*16);			\
  check##NAME##N ( &g6s##NAME##N,  6*16);			\
  check##NAME##N ( &g7s##NAME##N,  7*16);			\
  check##NAME##N ( &g8s##NAME##N,  8*16);			\
  check##NAME##N ( &g9s##NAME##N,  9*16);			\
  check##NAME##N (&g10s##NAME##N, 10*16);			\
  check##NAME##N (&g11s##NAME##N, 11*16);			\
  check##NAME##N (&g12s##NAME##N, 12*16);			\
  check##NAME##N (&g13s##NAME##N, 13*16);			\
  check##NAME##N (&g14s##NAME##N, 14*16);			\
  check##NAME##N (&g15s##NAME##N, 15*16);			\
  check##NAME##N (&g16s##NAME##N, 16*16);			\
}								\
								\
struct S##NAME##N						\
test0##NAME##N (void)						\
{								\
  return g1s##NAME##N;						\
}								\
								\
struct S##NAME##N						\
test1##NAME##N (struct S##NAME##N x01)				\
{								\
  return x01;							\
}								\
								\
struct S##NAME##N						\
testva##NAME##N (int n, ...)					\
{								\
  int i;							\
  struct S##NAME##N rslt;					\
  va_list ap;							\
  va_start (ap, n);						\
  for (i = 0; i < n; i++)					\
    rslt = va_arg (ap, struct S##NAME##N);			\
  va_end (ap);							\
  return rslt;							\
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
