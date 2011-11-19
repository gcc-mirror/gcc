/* { dg-do run } */

#include <stdlib.h>

#define N 1024
signed char sc[N];
short ss[N];
int si[N];
long long sl[N];
unsigned char uc[N];
unsigned short us[N];
unsigned int ui[N];
unsigned long long ul[N];
float f[N];
double d[N];

#define FN1(from, to) \
__attribute__((noinline, noclone)) void		\
from##2##to (void)				\
{						\
  int i;					\
  for (i = 0; i < N; i++)			\
    to[i] = from[i];				\
}
#define FN(intt, fltt) FN1 (intt, fltt) FN1 (fltt, intt)

FN (sc, f)
FN (ss, f)
FN (si, f)
FN (sl, f)
FN (uc, f)
FN (us, f)
FN (ui, f)
FN (ul, f)
FN (sc, d)
FN (ss, d)
FN (si, d)
FN (sl, d)
FN (uc, d)
FN (us, d)
FN (ui, d)
FN (ul, d)

#define FLTTEST(min, max, intt) \
__attribute__((noinline, noclone)) void					\
flttointtest##intt (void)						\
{									\
  int i;								\
  volatile float fltmin, fltmax, vf, vf2;				\
  volatile double dblmin, dblmax, vd, vd2;				\
  if (min == 0)								\
    fltmin = 0.0f;							\
  else									\
    {									\
      vf2 = fltmin = min - 1.0f;					\
      for (vf = 1.0f; (fltmin = vf2 + vf) == vf2; vf = vf * 2.0f)	\
	;								\
    }									\
  vf2 = fltmax = max + 1.0f;						\
  for (vf = 1.0f; (fltmax = vf2 - vf) == vf2; vf = vf * 2.0f)		\
    ;									\
  if (min == 0)								\
    dblmin = 0.0;							\
  else									\
    {									\
      vd2 = dblmin = min - 1.0;						\
      for (vd = 1.0; (dblmin = vd2 + vd) == vd2; vd = vd * 2.0)		\
	;								\
    }									\
  vd2 = dblmax = max + 1.0;						\
  for (vd = 1.0; (dblmax = vd2 - vd) == vd2; vd = vd * 2.0)		\
    ;									\
  for (i = 0; i < N; i++)						\
    {									\
      asm ("");								\
      if (i == 0)							\
	f[i] = fltmin;							\
      else if (i < N / 4)						\
	f[i] = fltmin + i + 0.25f;					\
      else if (i < 3 * N / 4)						\
	f[i] = (fltmax + fltmin) / 2.0 - N * 8 + 16.0f * i;		\
      else								\
	f[i] = fltmax - N + 1 + i;					\
      if (f[i] < fltmin) f[i] = fltmin;					\
      if (f[i] > fltmax) f[i] = fltmax;					\
      if (i == 0)							\
	d[i] = dblmin;							\
      else if (i < N / 4)						\
	d[i] = dblmin + i + 0.25f;					\
      else if (i < 3 * N / 4)						\
	d[i] = (dblmax + dblmin) / 2.0 - N * 8 + 16.0f * i;		\
      else								\
	d[i] = dblmax - N + 1 + i;					\
      if (d[i] < dblmin) d[i] = dblmin;					\
      if (d[i] > dblmax) d[i] = dblmax;					\
    }									\
  f2##intt ();								\
  for (i = 0; i < N; i++)						\
    if (intt[i] != (__typeof (intt[0])) f[i])				\
      abort ();								\
  d2##intt ();								\
  for (i = 0; i < N; i++)						\
    if (intt[i] != (__typeof (intt[0])) d[i])				\
      abort ();								\
  for (i = 0; i < N; i++)						\
    {									\
      unsigned long long r = random ();					\
      r = (r << 21) ^ (unsigned) random ();				\
      r = (r << 21) ^ (unsigned) random ();				\
      asm ("");								\
      f[i] = (r >> 59) / 32.0f + (__typeof (intt[0])) r;		\
      if (f[i] < fltmin) f[i] = fltmin;					\
      if (f[i] > fltmax) f[i] = fltmax;					\
      d[i] = (r >> 59) / 32.0 + (__typeof (intt[0])) r;			\
      if (d[i] < dblmin) f[i] = dblmin;					\
      if (d[i] > dblmax) f[i] = dblmax;					\
    }									\
  f2##intt ();								\
  for (i = 0; i < N; i++)						\
    if (intt[i] != (__typeof (intt[0])) f[i])				\
      abort ();								\
  d2##intt ();								\
  for (i = 0; i < N; i++)						\
    if (intt[i] != (__typeof (intt[0])) d[i])				\
      abort ();								\
}									\
									\
__attribute__((noinline, noclone)) void					\
inttoflttest##intt (void)						\
{									\
  int i;								\
  volatile float vf;							\
  volatile double vd;							\
  for (i = 0; i < N; i++)						\
    {									\
      asm ("");								\
      if (i < N / 4)							\
	intt[i] = min + i;						\
      else if (i < 3 * N / 4)						\
	intt[i] = (max + min) / 2 - N * 8 + 16 * i;			\
      else								\
	intt[i] = max - N + 1 + i;					\
    }									\
  intt##2f ();								\
  for (i = 0; i < N; i++)						\
    {									\
      vf = intt[i];							\
      if (f[i] != vf)							\
	abort ();							\
    }									\
  intt##2d ();								\
  for (i = 0; i < N; i++)						\
    {									\
      vd = intt[i];							\
      if (d[i] != vd)							\
	abort ();							\
    }									\
  for (i = 0; i < N; i++)						\
    {									\
      unsigned long long r = random ();					\
      r = (r << 21) ^ (unsigned) random ();				\
      r = (r << 21) ^ (unsigned) random ();				\
      asm ("");								\
      intt[i] = r;							\
    }									\
  intt##2f ();								\
  for (i = 0; i < N; i++)						\
    {									\
      vf = intt[i];							\
      if (f[i] != vf)							\
	abort ();							\
    }									\
  intt##2d ();								\
  for (i = 0; i < N; i++)						\
    {									\
      vd = intt[i];							\
      if (d[i] != vd)							\
	abort ();							\
    }									\
}

FLTTEST (- __SCHAR_MAX__ - 1, __SCHAR_MAX__, sc)
FLTTEST (- __SHRT_MAX__ - 1, __SHRT_MAX__, ss)
FLTTEST (- __INT_MAX__ - 1, __INT_MAX__, si)
FLTTEST (- __LONG_LONG_MAX__ - 1LL, __LONG_LONG_MAX__, sl)
FLTTEST (0, 2U * __SCHAR_MAX__ + 1, uc)
FLTTEST (0, 2U * __SHRT_MAX__ + 1, us)
FLTTEST (0, 2U * __INT_MAX__ + 1, ui)
FLTTEST (0, 2ULL * __LONG_LONG_MAX__ + 1, ul)

int
main ()
{
  flttointtestsc ();
  flttointtestss ();
  flttointtestsi ();
  flttointtestsl ();
  flttointtestuc ();
  flttointtestus ();
  flttointtestui ();
  flttointtestul ();
  inttoflttestsc ();
  inttoflttestss ();
  inttoflttestsi ();
  inttoflttestsl ();
  inttoflttestuc ();
  inttoflttestus ();
  inttoflttestui ();
  inttoflttestul ();
  return 0;
}
