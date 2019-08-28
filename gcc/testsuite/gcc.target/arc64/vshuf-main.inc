/* Driver fragment for __builtin_shuffle of any vector shape.  */

extern void abort (void);

#ifndef UNSUPPORTED
V a, b, c, d;

#define T(num, msk...) \
__attribute__((noinline, noclone)) void		\
test_##num (void)				\
{						\
  VI mask = { msk };				\
  int i;					\
  c = __builtin_shuffle (a, mask);		\
  d = __builtin_shuffle (a, b, mask);		\
  __asm ("" : : "r" (&c), "r" (&d) : "memory");	\
  for (i = 0; i < N; ++i)			\
    if (c[i] != a[mask[i] & (N - 1)])		\
      abort ();					\
    else if ((mask[i] & N))			\
      {						\
	if (d[i] != b[mask[i] & (N - 1)])	\
	  abort ();				\
      }						\
    else if (d[i] != a[mask[i] & (N - 1)])	\
      abort ();					\
}
TESTS
#ifdef EXPENSIVE
EXPTESTS
#endif
#endif

int
main ()
{
#ifndef UNSUPPORTED
  int i;
  for (i = 0; i < N; ++i)
    {
      a[i] = i + 2;
      b[i] = N + i + 2;
    }

#undef T
#define T(num, msk...) \
  test_##num ();
  TESTS
#ifdef EXPENSIVE
  EXPTESTS
#endif
#endif

  return 0;
}
