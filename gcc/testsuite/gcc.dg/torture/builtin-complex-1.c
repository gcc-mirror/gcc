/* Test __builtin_complex semantics.  */
/* { dg-do run } */
/* { dg-options "-std=c1x -pedantic-errors" } */
/* { dg-add-options ieee } */

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
comparef (float a, float b)
{
  COMPARE_BODY (a, b, float, __builtin_copysignf);
}

void
compare (double a, double b)
{
  COMPARE_BODY (a, b, double, __builtin_copysign);
}

void
comparel (long double a, long double b)
{
  COMPARE_BODY (a, b, long double, __builtin_copysignl);
}

void
comparecf (_Complex float a, float r, float i)
{
  comparef (__real__ a, r);
  comparef (__imag__ a, i);
}

void
comparec (_Complex double a, double r, double i)
{
  compare (__real__ a, r);
  compare (__imag__ a, i);
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
    VERIFY (PZ, PZ, TYPE, COMPARE);			\
    VERIFY (PZ, NZ, TYPE, COMPARE);			\
    VERIFY (PZ, NAN, TYPE, COMPARE);			\
    VERIFY (PZ, INF, TYPE, COMPARE);			\
    VERIFY (NZ, PZ, TYPE, COMPARE);			\
    VERIFY (NZ, NZ, TYPE, COMPARE);			\
    VERIFY (NZ, NAN, TYPE, COMPARE);			\
    VERIFY (NZ, INF, TYPE, COMPARE);			\
    VERIFY (NAN, PZ, TYPE, COMPARE);			\
    VERIFY (NAN, NZ, TYPE, COMPARE);			\
    VERIFY (NAN, NAN, TYPE, COMPARE);			\
    VERIFY (NAN, INF, TYPE, COMPARE);			\
    VERIFY (INF, PZ, TYPE, COMPARE);			\
    VERIFY (INF, NZ, TYPE, COMPARE);			\
    VERIFY (INF, NAN, TYPE, COMPARE);			\
    VERIFY (INF, INF, TYPE, COMPARE);			\
  } while (0)

void
check_float (void)
{
  ALL_CHECKS (0.0f, -0.0f, __builtin_nanf(""), __builtin_inff(),
	      float, comparecf);
}

void
check_double (void)
{
  ALL_CHECKS (0.0, -0.0, __builtin_nan(""), __builtin_inf(),
	      double, comparec);
}

void
check_long_double (void)
{
  ALL_CHECKS (0.0l, -0.0l, __builtin_nanl(""), __builtin_infl(),
	      long double, comparecl);
}

int
main (void)
{
  check_float ();
  check_double ();
  check_long_double ();
  exit (0);
}
