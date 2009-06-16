/* Copyright (C) 2009  Free Software Foundation.

   Verify that folding of built-in complex math functions with
   constant arguments is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  January 28, 2009.  */

/* { dg-do link } */
/* { dg-require-effective-target mpc } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Return TRUE if the signs of floating point values X and Y are not
   equal.  This is important when comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1,(X)) != __builtin_copysignf(1,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1,(X)) != __builtin_copysign(1,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1,(X)) != __builtin_copysignl(1,(Y)))

/* Return TRUE if signs of the real parts, and the signs of the
   imaginary parts, of X and Y are not equal.  */
#define COMPLEX_CKSGN_F(X,Y) \
  (CKSGN_F(__real__ (X), __real__ (Y)) || CKSGN_F (__imag__ (X), __imag__ (Y)))
#define COMPLEX_CKSGN(X,Y) \
  (CKSGN(__real__ (X), __real__ (Y)) || CKSGN (__imag__ (X), __imag__ (Y)))
#define COMPLEX_CKSGN_L(X,Y) \
  (CKSGN_L(__real__ (X), __real__ (Y)) || CKSGN_L (__imag__ (X), __imag__ (Y)))

/* For complex numbers, test that FUNC(ARG) == (RES).  */
#define TESTIT_COMPLEX(FUNC, ARG, RES) do { \
  if (__builtin_##FUNC##f(ARG) != (RES) \
    || COMPLEX_CKSGN_F(__builtin_##FUNC##f(ARG), (RES))) \
      link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != (RES) \
    || COMPLEX_CKSGN(__builtin_##FUNC(ARG), (RES))) \
      link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG) != (RES) \
    || COMPLEX_CKSGN_L(__builtin_##FUNC##l(ARG), (RES))) \
      link_error(__LINE__); \
  } while (0)

/* Return TRUE if X differs from EXPECTED by more than 1%.  If
   EXPECTED is zero, then any difference may return TRUE.  We don't
   worry about signed zeros.  */
#define DIFF1PCT_F(X,EXPECTED) \
  (__builtin_fabsf((X)-(EXPECTED)) * 100 > __builtin_fabsf(EXPECTED))
#define DIFF1PCT(X,EXPECTED) \
  (__builtin_fabs((X)-(EXPECTED)) * 100 > __builtin_fabs(EXPECTED))
#define DIFF1PCT_L(X,EXPECTED) \
  (__builtin_fabsl((X)-(EXPECTED)) * 100 > __builtin_fabsl(EXPECTED))

/* Return TRUE if complex value X differs from EXPECTED by more than
   1% in either the real or imaginary parts.  */
#define COMPLEX_DIFF1PCT_F(X,EXPECTED) \
  (DIFF1PCT_F(__real__ (X), __real__ (EXPECTED)) \
   || DIFF1PCT_F(__imag__ (X), __imag__ (EXPECTED)))
#define COMPLEX_DIFF1PCT(X,EXPECTED) \
  (DIFF1PCT(__real__ (X), __real__ (EXPECTED)) \
   || DIFF1PCT(__imag__ (X), __imag__ (EXPECTED)))
#define COMPLEX_DIFF1PCT_L(X,EXPECTED) \
  (DIFF1PCT_L(__real__ (X), __real__ (EXPECTED)) \
   || DIFF1PCT_L(__imag__ (X), __imag__ (EXPECTED)))

/* Range test, for complex numbers check that FUNC(ARG) is within 1%
   of RES.  This is NOT a test for accuracy to the last-bit, we're
   merely checking that we get relatively sane results.  I.e. the GCC
   builtin is hooked up to the correct MPC function call.  We first
   check the magnitude and then the sign.  */
#define TESTIT_COMPLEX_R(FUNC, ARG, RES) do { \
  if (COMPLEX_DIFF1PCT_F (__builtin_##FUNC##f(ARG), (RES)) \
      || COMPLEX_CKSGN_F(__builtin_##FUNC##f(ARG), (RES))) \
    link_error(__LINE__); \
  if (COMPLEX_DIFF1PCT (__builtin_##FUNC(ARG), (RES)) \
      || COMPLEX_CKSGN(__builtin_##FUNC(ARG), (RES))) \
    link_error(__LINE__); \
  if (COMPLEX_DIFF1PCT (__builtin_##FUNC(ARG), (RES)) \
      || COMPLEX_CKSGN(__builtin_##FUNC(ARG), (RES))) \
    link_error(__LINE__); \
  } while (0)

int main (void)
{
  TESTIT_COMPLEX (csin, 0.0F, 0.0F);
  TESTIT_COMPLEX (csin, -0.0F, -0.0F);
  TESTIT_COMPLEX (csin, __builtin_conjf(0.0F), __builtin_conjf(0.0F));
  TESTIT_COMPLEX (csin, __builtin_conjf(-0.0F), __builtin_conjf(-0.0F));

  TESTIT_COMPLEX_R (csin, 3.45678F + 2.34567FI, -1.633059F - 4.917448FI);
  TESTIT_COMPLEX_R (csin, 3.45678F - 2.34567FI, -1.633059F + 4.917448FI);
  TESTIT_COMPLEX_R (csin, -3.45678F + 2.34567FI, 1.633059F - 4.917448FI);
  TESTIT_COMPLEX_R (csin, -3.45678F - 2.34567FI, 1.633059F + 4.917448FI);
  
  TESTIT_COMPLEX (ccos, 0.0F, __builtin_conjf(1.0F));
  TESTIT_COMPLEX (ccos, -0.0F, 1.0F);
  TESTIT_COMPLEX (ccos, __builtin_conjf(0.0F), 1.0F);
  TESTIT_COMPLEX (ccos, __builtin_conjf(-0.0F), __builtin_conjf(1.0F));

  TESTIT_COMPLEX_R (ccos, 3.45678F + 2.34567FI, -5.008512F + 1.603367FI);
  TESTIT_COMPLEX_R (ccos, 3.45678F - 2.34567FI, -5.008512F - 1.603367FI);
  TESTIT_COMPLEX_R (ccos, -3.45678F + 2.34567FI, -5.008512F - 1.603367FI);
  TESTIT_COMPLEX_R (ccos, -3.45678F - 2.34567FI, -5.008512F + 1.603367FI);

  TESTIT_COMPLEX (ctan, 0.0F, 0.0F);
  TESTIT_COMPLEX (ctan, -0.0F, -0.0F);
  TESTIT_COMPLEX (ctan, __builtin_conjf(0.0F), __builtin_conjf(0.0F));
  TESTIT_COMPLEX (ctan, __builtin_conjf(-0.0F), __builtin_conjf(-0.0F));

  TESTIT_COMPLEX_R (ctan, 3.45678F + 2.34567FI, 0.010657F + 0.985230FI);
  TESTIT_COMPLEX_R (ctan, 3.45678F - 2.34567FI, 0.010657F - 0.985230FI);
  TESTIT_COMPLEX_R (ctan, -3.45678F + 2.34567FI, -0.010657F + 0.985230FI);
  TESTIT_COMPLEX_R (ctan, -3.45678F - 2.34567FI, -0.010657F - 0.985230FI);

  TESTIT_COMPLEX (csinh, 0.0F, 0.0F);
  TESTIT_COMPLEX (csinh, -0.0F, -0.0F);
  TESTIT_COMPLEX (csinh, __builtin_conjf(0.0F), __builtin_conjf(0.0F));
  TESTIT_COMPLEX (csinh, __builtin_conjf(-0.0F), __builtin_conjf(-0.0F));

  TESTIT_COMPLEX_R (csinh, 3.45678F + 2.34567FI, -11.083178F + 11.341487FI);
  TESTIT_COMPLEX_R (csinh, 3.45678F - 2.34567FI, -11.083178F - 11.341487FI);
  TESTIT_COMPLEX_R (csinh, -3.45678F + 2.34567FI, 11.083178F + 11.341487FI);
  TESTIT_COMPLEX_R (csinh, -3.45678F - 2.34567FI, 11.083178F - 11.341487FI);

  TESTIT_COMPLEX (ccosh, 0.0F, 1.0F);
  TESTIT_COMPLEX (ccosh, -0.0F, __builtin_conjf(1.0F));
  TESTIT_COMPLEX (ccosh, __builtin_conjf(0.0F), __builtin_conjf(1.0F));
  TESTIT_COMPLEX (ccosh, __builtin_conjf(-0.0F), 1.0F);

  TESTIT_COMPLEX_R (ccosh, 3.45678F + 2.34567FI, -11.105238F + 11.318958FI);
  TESTIT_COMPLEX_R (ccosh, 3.45678F - 2.34567FI, -11.105238F - 11.318958FI);
  TESTIT_COMPLEX_R (ccosh, -3.45678F + 2.34567FI, -11.105238F - 11.318958FI);
  TESTIT_COMPLEX_R (ccosh, -3.45678F - 2.34567FI, -11.105238F + 11.318958FI);

  TESTIT_COMPLEX (ctanh, 0.0F, 0.0F);
  TESTIT_COMPLEX (ctanh, -0.0F, -0.0F);
  TESTIT_COMPLEX (ctanh, __builtin_conjf(0.0F), __builtin_conjf(0.0F));
  TESTIT_COMPLEX (ctanh, __builtin_conjf(-0.0F), __builtin_conjf(-0.0F));

  TESTIT_COMPLEX_R (ctanh, 3.45678F + 2.34567FI, 1.000040F - 0.001988FI);
  TESTIT_COMPLEX_R (ctanh, 3.45678F - 2.34567FI, 1.000040F + 0.001988FI);
  TESTIT_COMPLEX_R (ctanh, -3.45678F + 2.34567FI, -1.000040F - 0.001988FI);
  TESTIT_COMPLEX_R (ctanh, -3.45678F - 2.34567FI, -1.000040F + 0.001988FI);

  TESTIT_COMPLEX (clog, 1.0F, 0.0F);
  TESTIT_COMPLEX_R (clog, -1.0F, 3.141593FI);
  TESTIT_COMPLEX (clog, __builtin_conjf(1.0F), __builtin_conjf(0.0F)); /* Fails with mpc-0.6.  */
  TESTIT_COMPLEX_R (clog, __builtin_conjf(-1.0F), __builtin_conjf(3.141593FI)); /* Fails with mpc-0.6.  */

  TESTIT_COMPLEX_R (clog, 3.45678F + 2.34567FI, 1.429713F + 0.596199FI);
  TESTIT_COMPLEX_R (clog, 3.45678F - 2.34567FI, 1.429713F - 0.596199FI);
  TESTIT_COMPLEX_R (clog, -3.45678F + 2.34567FI, 1.429713F + 2.545394FI);
  TESTIT_COMPLEX_R (clog, -3.45678F - 2.34567FI, 1.429713F - 2.545394FI);

  TESTIT_COMPLEX (csqrt, 0.0F, 0.0F);
  TESTIT_COMPLEX (csqrt, -0.0F, 0.0F);
  TESTIT_COMPLEX (csqrt, __builtin_conjf(0.0F), __builtin_conjf(0.0F));
  TESTIT_COMPLEX (csqrt, __builtin_conjf(-0.0F), __builtin_conjf(0.0F));

  TESTIT_COMPLEX_R (csqrt, 3.45678F + 2.34567FI, 1.953750F + 0.600299FI);
  TESTIT_COMPLEX_R (csqrt, 3.45678F - 2.34567FI, 1.953750F - 0.600299FI);
  TESTIT_COMPLEX_R (csqrt, -3.45678F + 2.34567FI, 0.600299F + 1.953750FI);
  TESTIT_COMPLEX_R (csqrt, -3.45678F - 2.34567FI, 0.600299F - 1.953750FI);

  return 0;
}
