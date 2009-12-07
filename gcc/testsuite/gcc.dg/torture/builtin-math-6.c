/* Copyright (C) 2009  Free Software Foundation.

   Verify that folding of built-in complex math functions with
   constant arguments is correctly performed by the compiler.

   Origin: Kaveh R. Ghazi,  January 28, 2009.  */

/* { dg-do link } */

/* All references to link_error should go away at compile-time.  The
   first number is the line number and the second is the value number
   among several tests.  These appear in the tree dump file and aid in
   debugging.  */
extern void link_error(int, int);

#define CONJ(X) __builtin_conjf(X)

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
#define TESTIT_COMPLEX(VAL_NUM, FUNC, ARG, RES) do { \
  if (__builtin_##FUNC##f(ARG) != (RES) \
    || COMPLEX_CKSGN_F(__builtin_##FUNC##f(ARG), (RES))) \
      link_error(__LINE__, VAL_NUM); \
  if (__builtin_##FUNC(ARG) != (RES) \
    || COMPLEX_CKSGN(__builtin_##FUNC(ARG), (RES))) \
      link_error(__LINE__, VAL_NUM); \
  if (__builtin_##FUNC##l(ARG) != (RES) \
    || COMPLEX_CKSGN_L(__builtin_##FUNC##l(ARG), (RES))) \
      link_error(__LINE__, VAL_NUM); \
  } while (0)

/* For complex numbers, call the TESTIT_COMPLEX macro for all
   combinations of neg and conj.  */
#define TESTIT_COMPLEX_ALLNEG(FUNC, ARG, RES1, RES2, RES3, RES4) do { \
  TESTIT_COMPLEX(1, FUNC, (_Complex float)(ARG), RES1); \
  TESTIT_COMPLEX(2, FUNC, -CONJ(ARG), RES2); \
  TESTIT_COMPLEX(3, FUNC, CONJ(ARG), RES3); \
  TESTIT_COMPLEX(4, FUNC, -(_Complex float)(ARG), RES4); \
} while (0)

/* For complex numbers, call the TESTIT_COMPLEX_R macro for all
   combinations of neg and conj.  */
#define TESTIT_COMPLEX_R_ALLNEG(FUNC, ARG, RES1, RES2, RES3, RES4) do { \
  TESTIT_COMPLEX_R(1, FUNC, (_Complex float)(ARG), RES1); \
  TESTIT_COMPLEX_R(2, FUNC, -CONJ(ARG), RES2); \
  TESTIT_COMPLEX_R(3, FUNC, CONJ(ARG), RES3); \
  TESTIT_COMPLEX_R(4, FUNC, -(_Complex float)(ARG), RES4); \
} while (0)

/* For complex numbers, test that FUNC(ARG0, ARG1) == (RES).  */
#define TESTIT_COMPLEX2(VAL_NUM, FUNC, ARG0, ARG1, RES) do { \
  if (__builtin_##FUNC##f(ARG0, ARG1) != (RES) \
    || COMPLEX_CKSGN_F(__builtin_##FUNC##f(ARG0, ARG1), (RES))) \
      link_error(__LINE__, VAL_NUM); \
  if (__builtin_##FUNC(ARG0, ARG1) != (RES) \
    || COMPLEX_CKSGN(__builtin_##FUNC(ARG0, ARG1), (RES))) \
      link_error(__LINE__, VAL_NUM); \
  if (__builtin_##FUNC##l(ARG0, ARG1) != (RES) \
    || COMPLEX_CKSGN_L(__builtin_##FUNC##l(ARG0, ARG1), (RES))) \
      link_error(__LINE__, VAL_NUM); \
  } while (0)

/* For complex numbers, call the TESTIT_COMPLEX2 macro for all
   combinations of neg and conj.  */
#define TESTIT_COMPLEX2_ALLNEG(FUNC, ARG0, ARG1, RES1, RES2, RES3, RES4, RES5,\
 RES6, RES7, RES8, RES9, RES10, RES11, RES12, RES13, RES14, RES15, RES16) do{ \
  TESTIT_COMPLEX2(1, FUNC, (_Complex float)(ARG0),(_Complex float)(ARG1), RES1);\
  TESTIT_COMPLEX2(2, FUNC, (_Complex float)(ARG0),CONJ(ARG1), RES2); \
  TESTIT_COMPLEX2(3, FUNC, (_Complex float)(ARG0),-(_Complex float)(ARG1), RES3); \
  TESTIT_COMPLEX2(4, FUNC, (_Complex float)(ARG0),-CONJ(ARG1), RES4); \
  TESTIT_COMPLEX2(5, FUNC, -(_Complex float)(ARG0),(_Complex float)(ARG1), RES5); \
  TESTIT_COMPLEX2(6, FUNC, -(_Complex float)(ARG0),CONJ(ARG1), RES6); \
  TESTIT_COMPLEX2(7, FUNC, -(_Complex float)(ARG0),-(_Complex float)(ARG1), RES7); \
  TESTIT_COMPLEX2(8, FUNC, -(_Complex float)(ARG0),-CONJ(ARG1), RES8); \
  TESTIT_COMPLEX2(9, FUNC, CONJ(ARG0),(_Complex float)(ARG1), RES9); \
  TESTIT_COMPLEX2(10, FUNC, CONJ(ARG0),CONJ(ARG1), RES10); \
  TESTIT_COMPLEX2(11, FUNC, CONJ(ARG0),-(_Complex float)(ARG1), RES11); \
  TESTIT_COMPLEX2(12, FUNC, CONJ(ARG0),-CONJ(ARG1), RES12); \
  TESTIT_COMPLEX2(13, FUNC, -CONJ(ARG0),(_Complex float)(ARG1), RES13); \
  TESTIT_COMPLEX2(14, FUNC, -CONJ(ARG0),CONJ(ARG1), RES14); \
  TESTIT_COMPLEX2(15, FUNC, -CONJ(ARG0),-(_Complex float)(ARG1), RES15); \
  TESTIT_COMPLEX2(16, FUNC, -CONJ(ARG0),-CONJ(ARG1), RES16); \
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
#define TESTIT_COMPLEX_R(VAL_NUM, FUNC, ARG, RES) do { \
  if (COMPLEX_DIFF1PCT_F (__builtin_##FUNC##f(ARG), (RES)) \
      || COMPLEX_CKSGN_F(__builtin_##FUNC##f(ARG), (RES))) \
    link_error(__LINE__, VAL_NUM); \
  if (COMPLEX_DIFF1PCT (__builtin_##FUNC(ARG), (RES)) \
      || COMPLEX_CKSGN(__builtin_##FUNC(ARG), (RES))) \
    link_error(__LINE__, VAL_NUM); \
  if (COMPLEX_DIFF1PCT (__builtin_##FUNC(ARG), (RES)) \
      || COMPLEX_CKSGN(__builtin_##FUNC(ARG), (RES))) \
    link_error(__LINE__, VAL_NUM); \
  } while (0)

/* Range test, for complex numbers check that FUNC(ARG0, ARG1) is
   within 1% of RES.  This is NOT a test for accuracy to the last-bit,
   we're merely checking that we get relatively sane results.
   I.e. the GCC builtin is hooked up to the correct MPC function call.
   We first check the magnitude and then the sign.  */
#define TESTIT_COMPLEX_R2(VAL_NUM, FUNC, ARG0, ARG1, RES) do { \
  if (COMPLEX_DIFF1PCT_F (__builtin_##FUNC##f(ARG0, ARG1), (RES)) \
      || COMPLEX_CKSGN_F (__builtin_##FUNC##f(ARG0, ARG1), (RES))) \
    link_error(__LINE__, VAL_NUM); \
  if (COMPLEX_DIFF1PCT (__builtin_##FUNC(ARG0, ARG1), (RES)) \
      || COMPLEX_CKSGN (__builtin_##FUNC(ARG0, ARG1), (RES))) \
    link_error(__LINE__, VAL_NUM); \
  if (COMPLEX_DIFF1PCT_L (__builtin_##FUNC##l(ARG0, ARG1), (RES)) \
      || COMPLEX_CKSGN_L (__builtin_##FUNC##l(ARG0, ARG1), (RES))) \
    link_error(__LINE__, VAL_NUM); \
  } while (0)

/* For complex numbers, call the TESTIT_COMPLEX_R2 macro for all
   combinations of neg and conj.  */
#define TESTIT_COMPLEX_R2_ALLNEG(FUNC, ARG0, ARG1, RES1, RES2, RES3, RES4, RES5,\
 RES6, RES7, RES8, RES9, RES10, RES11, RES12, RES13, RES14, RES15, RES16) do{ \
  TESTIT_COMPLEX_R2(1, FUNC, (_Complex float)(ARG0),(_Complex float)(ARG1), RES1);\
  TESTIT_COMPLEX_R2(2, FUNC, (_Complex float)(ARG0),CONJ(ARG1), RES2); \
  TESTIT_COMPLEX_R2(3, FUNC, (_Complex float)(ARG0),-(_Complex float)(ARG1), RES3); \
  TESTIT_COMPLEX_R2(4, FUNC, (_Complex float)(ARG0),-CONJ(ARG1), RES4); \
  TESTIT_COMPLEX_R2(5, FUNC, -(_Complex float)(ARG0),(_Complex float)(ARG1), RES5); \
  TESTIT_COMPLEX_R2(6, FUNC, -(_Complex float)(ARG0),CONJ(ARG1), RES6); \
  TESTIT_COMPLEX_R2(7, FUNC, -(_Complex float)(ARG0),-(_Complex float)(ARG1), RES7); \
  TESTIT_COMPLEX_R2(8, FUNC, -(_Complex float)(ARG0),-CONJ(ARG1), RES8); \
  TESTIT_COMPLEX_R2(9, FUNC, CONJ(ARG0),(_Complex float)(ARG1), RES9); \
  TESTIT_COMPLEX_R2(10, FUNC, CONJ(ARG0),CONJ(ARG1), RES10); \
  TESTIT_COMPLEX_R2(11, FUNC, CONJ(ARG0),-(_Complex float)(ARG1), RES11); \
  TESTIT_COMPLEX_R2(12, FUNC, CONJ(ARG0),-CONJ(ARG1), RES12); \
  TESTIT_COMPLEX_R2(13, FUNC, -CONJ(ARG0),(_Complex float)(ARG1), RES13); \
  TESTIT_COMPLEX_R2(14, FUNC, -CONJ(ARG0),CONJ(ARG1), RES14); \
  TESTIT_COMPLEX_R2(15, FUNC, -CONJ(ARG0),-(_Complex float)(ARG1), RES15); \
  TESTIT_COMPLEX_R2(16, FUNC, -CONJ(ARG0),-CONJ(ARG1), RES16); \
} while (0)

int main (void)
{
  TESTIT_COMPLEX (1, cacos, 1, CONJ(0));
  TESTIT_COMPLEX_R (1, cacos, -1, CONJ(3.141593F));
  TESTIT_COMPLEX (1, cacos, CONJ(1), 0);
  TESTIT_COMPLEX_R (1, cacos, CONJ(-1), 3.141593F);
  TESTIT_COMPLEX_R_ALLNEG (cacos, 3.45678F + 2.34567FI,
			   0.60971F - 2.11780FI, 2.531875F - 2.117800FI,
			   0.60971F + 2.11780FI, 2.531875F + 2.117800FI);

  TESTIT_COMPLEX_ALLNEG (casin, 0,
			 0, -CONJ(0), CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (casin, 3.45678F + 2.34567FI,
			   0.96107F + 2.11780FI, -0.96107F + 2.11780FI,
			   0.96107F - 2.11780FI, -0.96107F - 2.11780FI);

  TESTIT_COMPLEX_ALLNEG (catan, 0,
			 0, -CONJ(0), CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (catan, 3.45678F + 2.34567FI,
			   1.37188F + 0.12997FI, -1.37188F + 0.12997FI,
			   1.37188F - 0.12997FI, -1.37188F - 0.12997FI);

  TESTIT_COMPLEX (1, cacosh, 1, 0);
  TESTIT_COMPLEX_R (1, cacosh, -1, 3.141593FI);
  TESTIT_COMPLEX (1, cacosh, CONJ(1), CONJ(0));
  TESTIT_COMPLEX_R (1, cacosh, CONJ(-1), CONJ(3.141593FI));
  TESTIT_COMPLEX_R_ALLNEG (cacosh, 3.45678F + 2.34567FI,
			   2.11780F + 0.60971FI, 2.11780F + 2.531875FI,
			   2.11780F - 0.60971FI, 2.11780F - 2.531875FI);

  TESTIT_COMPLEX_ALLNEG (casinh, 0,
			 0, -CONJ(0), CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (casinh, 3.45678F + 2.34567FI,
			   2.12836F + 0.58310FI, -2.12836F + 0.58310FI,
			   2.12836F - 0.58310FI, -2.12836F - 0.58310FI);

  TESTIT_COMPLEX_ALLNEG (catanh, 0,
			 0, -CONJ(0), CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (catanh, 3.45678F + 2.34567FI,
			   0.19693F + 1.43190FI, -0.19693F + 1.43190FI,
			   0.19693F - 1.43190FI, -0.19693F - 1.43190FI);

  TESTIT_COMPLEX_ALLNEG (csin, 0,
			 0, -0.F, CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (csin, 3.45678F + 2.34567FI,
			   -1.633059F - 4.917448FI, 1.633059F - 4.917448FI,
			   -1.633059F + 4.917448FI, 1.633059F + 4.917448FI);

  TESTIT_COMPLEX_ALLNEG (ccos, 0,
			 CONJ(1), 1, 1, CONJ(1));
  TESTIT_COMPLEX_R_ALLNEG (ccos, 3.45678F + 2.34567FI,
			   -5.008512F + 1.603367FI, -5.008512F - 1.603367FI,
			   -5.008512F - 1.603367FI, -5.008512F + 1.603367FI);

  TESTIT_COMPLEX_ALLNEG (ctan, 0,
			 0, -0.F, CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (ctan, 3.45678F + 2.34567FI,
			   0.010657F + 0.985230FI, -0.010657F + 0.985230FI,
			   0.010657F - 0.985230FI, -0.010657F - 0.985230FI);
  
  TESTIT_COMPLEX_ALLNEG (csinh, 0,
			 0, -0.F, CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (csinh, 3.45678F + 2.34567FI,
			   -11.083178F + 11.341487FI, 11.083178F +11.341487FI,
			   -11.083178F - 11.341487FI, 11.083178F -11.341487FI);
  
  TESTIT_COMPLEX_ALLNEG (ccosh, 0,
			 1, CONJ(1), CONJ(1), 1);
  TESTIT_COMPLEX_R_ALLNEG (ccosh, 3.45678F + 2.34567FI,
			   -11.105238F + 11.318958FI,-11.105238F -11.318958FI,
			   -11.105238F - 11.318958FI,-11.105238F +11.318958FI);
  
  TESTIT_COMPLEX_ALLNEG (ctanh, 0,
			 0, -0.F, CONJ(0), CONJ(-0.F));
  TESTIT_COMPLEX_R_ALLNEG (ctanh, 3.45678F + 2.34567FI,
			   1.000040F - 0.001988FI, -1.000040F - 0.001988FI,
			   1.000040F + 0.001988FI, -1.000040F + 0.001988FI);

  TESTIT_COMPLEX (1, clog, 1, 0);
  TESTIT_COMPLEX_R (1, clog, -1, 3.141593FI);
  TESTIT_COMPLEX (1, clog, CONJ(1), CONJ(0));
  TESTIT_COMPLEX_R (1, clog, CONJ(-1), CONJ(3.141593FI));
  TESTIT_COMPLEX_R_ALLNEG (clog, 3.45678F + 2.34567FI,
			   1.429713F + 0.596199FI, 1.429713F + 2.545394FI,
			   1.429713F - 0.596199FI, 1.429713F - 2.545394FI);

  TESTIT_COMPLEX_ALLNEG (csqrt, 0,
			 0, 0, CONJ(0), CONJ(0));
  TESTIT_COMPLEX_R_ALLNEG (csqrt, 3.45678F + 2.34567FI,
			   1.953750F + 0.600299FI, 0.600299F + 1.953750FI,
			   1.953750F - 0.600299FI, 0.600299F - 1.953750FI);
  
  TESTIT_COMPLEX2_ALLNEG (cpow, 1, 0,
			  1, 1, CONJ(1), CONJ(1), CONJ(1), CONJ(1), 1, 1,
			  CONJ(1), CONJ(1), 1, 1, 1, 1, CONJ(1), CONJ(1));
  TESTIT_COMPLEX2_ALLNEG (cpow, 1.FI, 0,
			  1, 1, CONJ(1), CONJ(1), CONJ(1), CONJ(1), 1, 1,
			  CONJ(1), CONJ(1), 1, 1, 1, 1, CONJ(1), CONJ(1));
  TESTIT_COMPLEX_R2_ALLNEG (cpow, 2, 3,
			    8, 8, CONJ(1/8.F), CONJ(1/8.F), CONJ(-8), CONJ(-8), -1/8.F, -1/8.F,
			    CONJ(8), CONJ(8), 1/8.F, 1/8.F, -8, -8, CONJ(-1/8.F), CONJ(-1/8.F));
  TESTIT_COMPLEX_R2_ALLNEG (cpow, 3, 4,
			    81, 81, CONJ(1/81.F), CONJ(1/81.F), CONJ(81), CONJ(81), 1/81.F, 1/81.F,
			    CONJ(81), CONJ(81), 1/81.F, 1/81.F, 81, 81, CONJ(1/81.F), CONJ(1/81.F));
  TESTIT_COMPLEX_R2_ALLNEG (cpow, 3, 5,
			    243, 243, CONJ(1/243.F), CONJ(1/243.F), CONJ(-243), CONJ(-243), -1/243.F, -1/243.F,
			    CONJ(243), CONJ(243), 1/243.F, 1/243.F, -243, -243, CONJ(-1/243.F), CONJ(-1/243.F));
  TESTIT_COMPLEX_R2_ALLNEG (cpow, 4, 2,
			    16, 16, CONJ(1/16.F), CONJ(1/16.F), CONJ(16), CONJ(16), 1/16.F, 1/16.F,
			    CONJ(16), CONJ(16), 1/16.F, 1/16.F, 16, 16, CONJ(1/16.F), CONJ(1/16.F));
  TESTIT_COMPLEX_R2_ALLNEG (cpow, 1.5, 3,
			    3.375F, 3.375F, CONJ(1/3.375F), CONJ(1/3.375F), CONJ(-3.375F), CONJ(-3.375F), -1/3.375F, -1/3.375F,
			    CONJ(3.375F), CONJ(3.375F), 1/3.375F, 1/3.375F, -3.375F, -3.375F, CONJ(-1/3.375F), CONJ(-1/3.375F));
  
  TESTIT_COMPLEX2 (1, cpow, 16, 0.25F, 2);

  TESTIT_COMPLEX_R2 (1, cpow, 3.45678F + 2.34567FI, 1.23456 + 4.56789FI, 0.212485F + 0.319304FI);
  TESTIT_COMPLEX_R2 (1, cpow, 3.45678F - 2.34567FI, 1.23456 + 4.56789FI, 78.576402F + -41.756208FI);
  TESTIT_COMPLEX_R2 (1, cpow, -1.23456F + 2.34567FI, 2.34567 - 1.23456FI, -110.629847F + -57.021655FI);
  TESTIT_COMPLEX_R2 (1, cpow, -1.23456F - 2.34567FI, 2.34567 - 1.23456FI, 0.752336F + 0.199095FI);
  
  return 0;
}
