/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O3 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

extern void abort (void);
double ad[64], bd[64], cd[64], dd[64], ed[64];
float af[64], bf[64], cf[64], df[64], ef[64];
signed char ac[64], bc[64], cc[64], dc[64], ec[64];
short as[64], bs[64], cs[64], ds[64], es[64];
int ai[64], bi[64], ci[64], di[64], ei[64];
long long all[64], bll[64], cll[64], dll[64], ell[64];
unsigned char auc[64], buc[64], cuc[64], duc[64], euc[64];
unsigned short aus[64], bus[64], cus[64], dus[64], eus[64];
unsigned int au[64], bu[64], cu[64], du[64], eu[64];
unsigned long long aull[64], bull[64], cull[64], dull[64], eull[64];

#define F(var) \
__attribute__((noinline, noclone)) void \
f##var (void) \
{ \
  int i; \
  for (i = 0; i < 64; i++) \
    { \
      __typeof (a##var[0]) d = d##var[i], e = e##var[i]; \
      a##var[i] = b##var[i] > c##var[i] ? d : e; \
    } \
}

#define TESTS \
F (d) F (f) F (c) F (s) F (i) F (ll) F (uc) F (us) F (u) F (ull)

TESTS

void
TEST ()
{
  int i;
  for (i = 0; i < 64; i++)
    {
#undef F
#define F(var) \
      b##var[i] = i + 64; \
      switch (i % 3) \
	{ \
	case 0: c##var[i] = i + 64; break; \
	case 1: c##var[i] = 127 - i; break; \
	case 2: c##var[i] = i; break; \
	} \
      d##var[i] = i / 2; \
      e##var[i] = i * 2;
      TESTS
    }
#undef F
#define F(var) f##var ();
  TESTS
  for (i = 0; i < 64; i++)
    {
      asm volatile ("" : : : "memory");
#undef F
#define F(var) \
      if (a##var[i] != (b##var[i] > c##var[i] ? d##var[i] : e##var[i])) \
	abort ();
      TESTS
    }
}
