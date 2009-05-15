/* Copyright (C) 2009  Free Software Foundation.

   Verify that folding of built-in complex math functions with
   constant arguments is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  January 28, 2009.  */

/* { dg-do link } */
/* { dg-require-effective-target mpc } */

/* All references to link_error should go away at compile-time.  */
extern void link_error(int);

/* Return TRUE if the sign of X != sign of Y.  This is important when
   comparing signed zeros.  */
#define CKSGN_F(X,Y) \
  (__builtin_copysignf(1.0F,(X)) != __builtin_copysignf(1.0F,(Y)))
#define CKSGN(X,Y) \
  (__builtin_copysign(1.0,(X)) != __builtin_copysign(1.0,(Y)))
#define CKSGN_L(X,Y) \
  (__builtin_copysignl(1.0L,(X)) != __builtin_copysignl(1.0L,(Y)))

/* For complex numbers, test that FUNC(ARG) == (RES).  */
#define TESTIT_COMPLEX(FUNC, ARG, RES) do { \
  if (__builtin_##FUNC##f(ARG) != (RES) \
    || CKSGN_F(__real__ __builtin_##FUNC##f(ARG), __real__ (RES)) \
    || CKSGN_F(__imag__ __builtin_##FUNC##f(ARG), __imag__ (RES))) \
      link_error(__LINE__); \
  if (__builtin_##FUNC(ARG) != (RES) \
    || CKSGN(__real__ __builtin_##FUNC(ARG), __real__ (RES)) \
    || CKSGN(__imag__ __builtin_##FUNC(ARG), __imag__ (RES))) \
      link_error(__LINE__); \
  if (__builtin_##FUNC##l(ARG) != (RES) \
    || CKSGN_L(__real__ __builtin_##FUNC##l(ARG), __real__ (RES)) \
    || CKSGN_L(__imag__ __builtin_##FUNC##l(ARG), __imag__ (RES))) \
      link_error(__LINE__); \
  } while (0)

/* Range test, for complex numbers check that FUNC(ARG) is within 1%
   of RES.  This is NOT a test for accuracy to the last-bit, we're
   merely checking that we get relatively sane results.  I.e. the GCC
   builtin is hooked up to the correct MPC function call.  We first
   check the magnitude and then the sign.  */
#define TESTIT_COMPLEX_R(FUNC, ARG, RES) do { \
  if (__builtin_fabsf(__real__ __builtin_##FUNC##f(ARG)) < __builtin_fabsf(__real__ (RES)) * 0.99F \
      || __builtin_fabsf(__real__ __builtin_##FUNC##f(ARG)) > __builtin_fabsf(__real__ (RES)) * 1.01F \
      || __builtin_fabsf(__imag__ __builtin_##FUNC##f(ARG)) < __builtin_fabsf(__imag__ (RES)) * 0.99F \
      || __builtin_fabsf(__imag__ __builtin_##FUNC##f(ARG)) > __builtin_fabsf(__imag__ (RES)) * 1.01F \
      || CKSGN_F(__real__ __builtin_##FUNC##f(ARG), __real__ (RES)) \
      || CKSGN_F(__imag__ __builtin_##FUNC##f(ARG), __imag__ (RES))) \
    link_error(__LINE__); \
  if (__builtin_fabs(__real__ __builtin_##FUNC(ARG)) < __builtin_fabs(__real__ (RES)) * 0.99F \
      || __builtin_fabs(__real__ __builtin_##FUNC(ARG)) > __builtin_fabs(__real__ (RES)) * 1.01F \
      || __builtin_fabs(__imag__ __builtin_##FUNC(ARG)) < __builtin_fabs(__imag__ (RES)) * 0.99F \
      || __builtin_fabs(__imag__ __builtin_##FUNC(ARG)) > __builtin_fabs(__imag__ (RES)) * 1.01F \
      || CKSGN(__real__ __builtin_##FUNC(ARG), __real__ (RES)) \
      || CKSGN(__imag__ __builtin_##FUNC(ARG), __imag__ (RES))) \
    link_error(__LINE__); \
  if (__builtin_fabsl(__real__ __builtin_##FUNC##l(ARG)) < __builtin_fabsl(__real__ (RES)) * 0.99F \
      || __builtin_fabsl(__real__ __builtin_##FUNC##l(ARG)) > __builtin_fabsl(__real__ (RES)) * 1.01F \
      || __builtin_fabsl(__imag__ __builtin_##FUNC##l(ARG)) < __builtin_fabsl(__imag__ (RES)) * 0.99F \
      || __builtin_fabsl(__imag__ __builtin_##FUNC##l(ARG)) > __builtin_fabsl(__imag__ (RES)) * 1.01F \
      || CKSGN_L(__real__ __builtin_##FUNC##l(ARG), __real__ (RES)) \
      || CKSGN_L(__imag__ __builtin_##FUNC##l(ARG), __imag__ (RES))) \
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
  TESTIT_COMPLEX (clog, __builtin_conjf(1.0F), 0.0F);
  TESTIT_COMPLEX_R (clog, __builtin_conjf(-1.0F), 3.141593FI);

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
