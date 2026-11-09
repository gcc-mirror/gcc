/* Test the high half of __builtin_clmulg on _BitInt types narrower than
   the mode they live in.  */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#define REF(N)								\
__attribute__((noipa)) void						\
ref##N (unsigned _BitInt(N) a, unsigned _BitInt(N) b,			\
	unsigned _BitInt(N) *lo, unsigned _BitInt(N) *hi)		\
{									\
  unsigned long long r = 0;						\
  for (int i = 0; i < N; i++)						\
    if ((b >> i) & 1)							\
      r ^= (unsigned long long) a << i;					\
  *lo = r;								\
  *hi = r >> N;								\
}									\
__attribute__((noipa)) void						\
tst##N (unsigned _BitInt(N) a, unsigned _BitInt(N) b,			\
	unsigned _BitInt(N) *lo, unsigned _BitInt(N) *hi)		\
{									\
  *lo = __builtin_clmulg (a, b, hi);					\
}									\
/* Operands with a small known range.  */				\
__attribute__((noipa)) void						\
tstr##N (unsigned _BitInt(N) a, unsigned _BitInt(N) b,			\
	 unsigned _BitInt(N) *lo, unsigned _BitInt(N) *hi)		\
{									\
  *lo = __builtin_clmulg ((unsigned _BitInt(N)) (a & 0xf),		\
			  (unsigned _BitInt(N)) (b & 0xf), hi);		\
}									\
__attribute__((noipa)) void						\
check##N (void)								\
{									\
  for (unsigned int i = 0; i < (1U << N); i++)				\
    for (unsigned int j = 0; j < (1U << N); j++)			\
      {									\
	unsigned _BitInt(N) a = i, b = j, l, h, rl, rh;			\
	tst##N (a, b, &l, &h);						\
	ref##N (a, b, &rl, &rh);					\
	if (l != rl || h != rh)						\
	  __builtin_abort ();						\
	tstr##N (a, b, &l, &h);						\
	ref##N (a & 0xf, b & 0xf, &rl, &rh);				\
	if (l != rl || h != rh)						\
	  __builtin_abort ();						\
      }									\
}

#if __BITINT_MAXWIDTH__ >= 8
REF (2)
REF (3)
REF (4)
REF (5)
REF (6)
REF (7)
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 8
  check2 ();
  check3 ();
  check4 ();
  check5 ();
  check6 ();
  check7 ();
#endif
  return 0;
}
