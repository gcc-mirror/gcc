/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-flive-patching=inline-clone -funroll-loops -fno-tree-forwprop -fno-expensive-optimizations -mstack-arg-probe -fcompare-debug" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

extern void exit (int);
extern void abort (void);

#define COMPARE_BODY(A, B, TYPE, COPYSIGN)				\
  do {									\
    TYPE s1 = COPYSIGN ((TYPE) 1.0, A);					\
    TYPE s2 = COPYSIGN ((TYPE) 1.0, B);					\
    if (s1 != s2)							\
      abort ();								\
    if ((__builtin_isnan (A) != 0) != (__builtin_isnan (B) != 0))	\
      abort ();								\
    if ((A != B) != (__builtin_isnan (A) != 0))				\
      abort ();								\
  } while (0)

void
comparel (long double a, long double b)
{
  COMPARE_BODY (a, b, long double, __builtin_copysignl);
}

void
comparecl (_Complex long double a, long double r, long double i)
{
  comparel (__real__ a, r);
  comparel (__imag__ a, i);
}

#define VERIFY(A, B, TYPE, COMPARE)			\
  do {							\
    TYPE a = A;						\
    TYPE b = B;						\
    _Complex TYPE cr = __builtin_complex (a, b);	\
    static _Complex TYPE cs = __builtin_complex (A, B);	\
    COMPARE (cr, A, B);					\
    COMPARE (cs, A, B);					\
  } while (0)

#define ALL_CHECKS(PZ, NZ, NAN, INF, TYPE, COMPARE)	\
  do {							\
    VERIFY (NAN, NZ, TYPE, COMPARE);			\
    VERIFY (INF, NZ, TYPE, COMPARE);			\
    VERIFY (INF, NAN, TYPE, COMPARE);			\
    VERIFY (INF, INF, TYPE, COMPARE);			\
  } while (0)

void
check_long_double (void)
{
  ALL_CHECKS (0.0l, -0.0l, __builtin_nanl(""), __builtin_infl(), long double, comparecl);
}
