/* Tests for _FloatN / _FloatNx types: compile and execution tests for
   valid code with complex arithmetic.  Before including this file,
   define WIDTH as the value N; define EXT to 1 for _FloatNx and 0 for
   _FloatN.  */

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
# define CST(C) CONCAT4 (C, f, WIDTH, x)
# define CSTI(C) CONCAT4 (C, if, WIDTH, x)
# define CSTI2(C) CONCAT4 (C, F, WIDTH, xi)
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define CSTI(C) CONCAT3 (C, if, WIDTH)
# define CSTI2(C) CONCAT4 (C, F, WIDTH, i)
#endif

extern void exit (int);
extern void abort (void);

volatile TYPE a = CST (1.0);
volatile _Complex TYPE b = CST (2.0) + CSTI (3.0);
volatile _Complex TYPE c = CST (2.0) + CSTI2 (3.0);
volatile _Complex TYPE d = __builtin_complex (CST (2.0), CST (3.0));

_Complex TYPE
fn (_Complex TYPE arg)
{
  return arg / 4;
}

int
main (void)
{
  volatile _Complex TYPE r;
  if (b != c)
    abort ();
  if (b != d)
    abort ();
  r = a + b;
  if (__real__ r != CST (3.0) || __imag__ r != CST (3.0))
    abort ();
  r += d;
  if (__real__ r != CST (5.0) || __imag__ r != CST (6.0))
    abort ();
  r -= a;
  if (__real__ r != CST (4.0) || __imag__ r != CST (6.0))
    abort ();
  r /= (a + a);
  if (__real__ r != CST (2.0) || __imag__ r != CST (3.0))
    abort ();
  r *= (a + a);
  if (__real__ r != CST (4.0) || __imag__ r != CST (6.0))
    abort ();
  r -= b;
  if (__real__ r != CST (2.0) || __imag__ r != CST (3.0))
    abort ();
  r *= r;
  if (__real__ r != -CST (5.0) || __imag__ r != CST (12.0))
    abort ();
  /* Division may not be exact, so round result before comparing.  */
  r /= b;
  r += __builtin_complex (CST (100.0), CST (100.0));
  r -= __builtin_complex (CST (100.0), CST (100.0));
  if (r != b)
    abort ();
  r = fn (r);
  if (__real__ r != CST (0.5) || __imag__ r != CST (0.75))
    abort ();
  exit (0);
}
