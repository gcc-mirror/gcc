/* { dg-do run { target arm*-*-symbianelf* arm*-*-eabi* } } */
/* { dg-options "" } */

/* This file tests most of the non-C++ run-time helper functions
   described in Section 4 of the "Run-Time ABI for the ARM
   Architecture".  These are basic tests; they do not try to validate
   all of the corner cases in these routines.  

   The functions not tested here are:

     __aeabi_cdcmpeq
     __aeabi_cdcmple
     __aeabi_cdrcmple
     __aeabi_cfcmpeq
     __aeabi_cfcmple
     __aeabi_cfrcmple
     __aeabi_ldivmod
     __aeabi_uldivmod
     __aeabi_idivmod
     __aeabi_uidivmod

   These functions have non-standard calling conventions that would
   require the use of inline assembly to test.  It would be good to
   add such tests, but they have not yet been implemented.  

   There are also no tests for the "division by zero", "memory copying,
   clearing, and setting" functions.  */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* All these functions are defined to use the base ABI, so use the
   attribute to ensure the tests use the base ABI to call them even
   when the VFP ABI is otherwise in effect.  */
#define PCS __attribute__((pcs("aapcs")))

#define decl_float(code, type)						\
  extern type __aeabi_ ## code ## add (type, type) PCS;			\
  extern type __aeabi_ ## code ## div (type, type) PCS;			\
  extern type __aeabi_ ## code ## mul (type, type) PCS;			\
  extern type __aeabi_ ## code ## neg (type) PCS;			\
  extern type __aeabi_ ## code ## rsub (type, type) PCS;		\
  extern type __aeabi_ ## code ## sub (type, type) PCS;			\
  extern int __aeabi_ ## code ## cmpeq (type, type) PCS;		\
  extern int __aeabi_ ## code ## cmplt (type, type) PCS;		\
  extern int __aeabi_ ## code ## cmple (type, type) PCS;		\
  extern int __aeabi_ ## code ## cmpge (type, type) PCS;		\
  extern int __aeabi_ ## code ## cmpgt (type, type) PCS;		\
  extern int __aeabi_ ## code ## cmpun (type, type) PCS;		\
  extern int __aeabi_ ## code ## 2iz (type) PCS;			\
  extern unsigned int __aeabi_ ## code ## 2uiz (type) PCS;		\
  extern long long __aeabi_ ## code ## 2lz (type) PCS;			\
  extern unsigned long long __aeabi_ ## code ## 2ulz (type) PCS;	\
  extern type __aeabi_i2 ## code (int) PCS;				\
  extern type __aeabi_ui2 ## code (int) PCS;				\
  extern type __aeabi_l2 ## code (long long) PCS;			\
  extern type __aeabi_ul2 ## code (unsigned long long) PCS;		\
									\
  type code ## zero = 0.0;						\
  type code ## one = 1.0;						\
  type code ## two = 2.0;						\
  type code ## four = 4.0;						\
  type code ## minus_one = -1.0;					\
  type code ## minus_two = -2.0;					\
  type code ## minus_four = -4.0;					\
  type code ## epsilon = 1E-32;						\
  type code ## NaN = 0.0 / 0.0;

decl_float (d, double)
decl_float (f, float)

extern float __aeabi_d2f (double) PCS;
extern double __aeabi_f2d (float) PCS;
extern long long __aeabi_lmul (long long, long long);
extern long long __aeabi_llsl (long long, int);
extern long long __aeabi_llsr (long long, int);
extern long long __aeabi_lasr (long long, int);
extern int __aeabi_lcmp (long long, long long);
extern int __aeabi_ulcmp (unsigned long long, unsigned long long);
extern int __aeabi_idiv (int, int);
extern unsigned int __aeabi_uidiv (unsigned int, unsigned int);
extern int __aeabi_uread4 (void *);
extern int __aeabi_uwrite4 (int, void *);
extern long long __aeabi_uread8 (void *);
extern long long __aeabi_uwrite8 (long long, void *);

#define eq(a, b, type, abs, epsilon, format)			\
  {								\
    type a1;							\
    type b1;							\
								\
    a1 = a;							\
    b1 = b;							\
    if (abs (a1 - b1) > epsilon)				\
    {								\
      fprintf (stderr, "%d: Test %s == %s\n", __LINE__, #a, #b);	\
      fprintf (stderr, "%d: " format " != " format "\n",	\
	       __LINE__, a1, b1);				\
      abort ();							\
    }								\
  }

#define ieq(a, b) eq (a, b, int, abs, 0, "%d")
#define ueq(a, b) eq (a, b, unsigned int, abs, 0, "%u")
#define leq(a, b) eq (a, b, long long, abs, 0, "%lld")
#define uleq(a, b) eq (a, b, unsigned long long, abs, 0, "%llu")
#define feq(a, b) eq (a, b, float, fabs, fepsilon, "%f")
#define deq(a, b) eq (a, b, double, fabs, depsilon, "%g")

#define NUM_CMP_VALUES 6

/* Values picked to cover a range of small, large, positive and negative.  */
static unsigned int cmp_val[NUM_CMP_VALUES] = 
{
  0,
  1,
  0x40000000,
  0x80000000,
  0xc0000000,
  0xffffffff
};

/* All combinations for each of the above values. */
#define ulcmp(l, s, m) \
    s, l, l, l, l, l,  m, s, l, l, l, l, \
    m, m, s, l, l, l,  m, m, m, s, l, l, \
    m, m, m, m, s, l,  m, m, m, m, m, s

#define lcmp(l, s, m) \
    s, l, l, m, m, m,  m, s, l, m, m, m, \
    m, m, s, m, m, m,  l, l, l, s, l, l, \
    l, l, l, m, s, l,  l, l, l, m, m, s

/* All combinations of the above for high/low words.  */
static int lcmp_results[] =
{
  lcmp(ulcmp(-1, -1, -1), ulcmp(-1, 0, 1), ulcmp(1, 1, 1))
};

static int ulcmp_results[] =
{
  ulcmp(ulcmp(-1, -1, -1), ulcmp(-1, 0, 1), ulcmp(1, 1, 1))
};

static int signof(int i)
{
  if (i < 0)
    return -1;

  if (i == 0)
    return 0;

  return 1;
}

int main () {
  unsigned char bytes[256];
  int i, j, k, n;
  int *result;

  /* Table 2.  Double-precision floating-point arithmetic.  */
  deq (__aeabi_dadd (dzero, done), done);
  deq (__aeabi_dadd (done, done), dtwo);
  deq (__aeabi_ddiv (dminus_four, dminus_two), dtwo);
  deq (__aeabi_ddiv (dminus_two, dtwo), dminus_one);
  deq (__aeabi_dmul (dtwo, dtwo), dfour);
  deq (__aeabi_dmul (dminus_one, dminus_two), dtwo);
  deq (__aeabi_dneg (dminus_one), done);
  deq (__aeabi_dneg (dfour), dminus_four);
  deq (__aeabi_drsub (done, dzero), dminus_one);
  deq (__aeabi_drsub (dtwo, dminus_two), dminus_four);
  deq (__aeabi_dsub (dzero, done), dminus_one);
  deq (__aeabi_dsub (dminus_two, dtwo), dminus_four);

  /* Table 3.  Double-precision floating-point comparisons.  */
  ieq (__aeabi_dcmpeq (done, done), 1);
  ieq (__aeabi_dcmpeq (done, dzero), 0);
  ieq (__aeabi_dcmpeq (dNaN, dzero), 0);
  ieq (__aeabi_dcmpeq (dNaN, dNaN), 0);

  ieq (__aeabi_dcmplt (dzero, done), 1);
  ieq (__aeabi_dcmplt (done, dzero), 0);
  ieq (__aeabi_dcmplt (dzero, dzero), 0);
  ieq (__aeabi_dcmplt (dzero, dNaN), 0);
  ieq (__aeabi_dcmplt (dNaN, dNaN), 0);

  ieq (__aeabi_dcmple (dzero, done), 1);
  ieq (__aeabi_dcmple (done, dzero), 0);
  ieq (__aeabi_dcmple (dzero, dzero), 1);
  ieq (__aeabi_dcmple (dzero, dNaN), 0);
  ieq (__aeabi_dcmple (dNaN, dNaN), 0);

  ieq (__aeabi_dcmpge (dzero, done), 0);
  ieq (__aeabi_dcmpge (done, dzero), 1);
  ieq (__aeabi_dcmpge (dzero, dzero), 1);
  ieq (__aeabi_dcmpge (dzero, dNaN), 0);
  ieq (__aeabi_dcmpge (dNaN, dNaN), 0);

  ieq (__aeabi_dcmpgt (dzero, done), 0);
  ieq (__aeabi_dcmpgt (done, dzero), 1);
  ieq (__aeabi_dcmplt (dzero, dzero), 0);
  ieq (__aeabi_dcmpgt (dzero, dNaN), 0);
  ieq (__aeabi_dcmpgt (dNaN, dNaN), 0);

  ieq (__aeabi_dcmpun (done, done), 0);
  ieq (__aeabi_dcmpun (done, dzero), 0);
  ieq (__aeabi_dcmpun (dNaN, dzero), 1);
  ieq (__aeabi_dcmpun (dNaN, dNaN), 1);

  /* Table 4.  Single-precision floating-point arithmetic.  */
  feq (__aeabi_fadd (fzero, fone), fone);
  feq (__aeabi_fadd (fone, fone), ftwo);
  feq (__aeabi_fdiv (fminus_four, fminus_two), ftwo);
  feq (__aeabi_fdiv (fminus_two, ftwo), fminus_one);
  feq (__aeabi_fmul (ftwo, ftwo), ffour);
  feq (__aeabi_fmul (fminus_one, fminus_two), ftwo);
  feq (__aeabi_fneg (fminus_one), fone);
  feq (__aeabi_fneg (ffour), fminus_four);
  feq (__aeabi_frsub (fone, fzero), fminus_one);
  feq (__aeabi_frsub (ftwo, fminus_two), fminus_four);
  feq (__aeabi_fsub (fzero, fone), fminus_one);
  feq (__aeabi_fsub (fminus_two, ftwo), fminus_four);

  /* Table 5.  Single-precision floating-point comparisons.  */
  ieq (__aeabi_fcmpeq (fone, fone), 1);
  ieq (__aeabi_fcmpeq (fone, fzero), 0);
  ieq (__aeabi_fcmpeq (fNaN, fzero), 0);
  ieq (__aeabi_fcmpeq (fNaN, fNaN), 0);

  ieq (__aeabi_fcmplt (fzero, fone), 1);
  ieq (__aeabi_fcmplt (fone, fzero), 0);
  ieq (__aeabi_fcmplt (fzero, fzero), 0);
  ieq (__aeabi_fcmplt (fzero, fNaN), 0);
  ieq (__aeabi_fcmplt (fNaN, fNaN), 0);

  ieq (__aeabi_fcmple (fzero, fone), 1);
  ieq (__aeabi_fcmple (fone, fzero), 0);
  ieq (__aeabi_fcmple (fzero, fzero), 1);
  ieq (__aeabi_fcmple (fzero, fNaN), 0);
  ieq (__aeabi_fcmple (fNaN, fNaN), 0);

  ieq (__aeabi_fcmpge (fzero, fone), 0);
  ieq (__aeabi_fcmpge (fone, fzero), 1);
  ieq (__aeabi_fcmpge (fzero, fzero), 1);
  ieq (__aeabi_fcmpge (fzero, fNaN), 0);
  ieq (__aeabi_fcmpge (fNaN, fNaN), 0);

  ieq (__aeabi_fcmpgt (fzero, fone), 0);
  ieq (__aeabi_fcmpgt (fone, fzero), 1);
  ieq (__aeabi_fcmplt (fzero, fzero), 0);
  ieq (__aeabi_fcmpgt (fzero, fNaN), 0);
  ieq (__aeabi_fcmpgt (fNaN, fNaN), 0);

  ieq (__aeabi_fcmpun (fone, fone), 0);
  ieq (__aeabi_fcmpun (fone, fzero), 0);
  ieq (__aeabi_fcmpun (fNaN, fzero), 1);
  ieq (__aeabi_fcmpun (fNaN, fNaN), 1);

  /* Table 6.  Floating-point to integer conversions.  */
  ieq (__aeabi_d2iz (dminus_one), -1);
  ueq (__aeabi_d2uiz (done), 1);
  leq (__aeabi_d2lz (dminus_two), -2LL);
  uleq (__aeabi_d2ulz (dfour), 4LL);
  ieq (__aeabi_f2iz (fminus_one), -1);
  ueq (__aeabi_f2uiz (fone), 1);
  leq (__aeabi_f2lz (fminus_two), -2LL);
  uleq (__aeabi_f2ulz (ffour), 4LL);

  /* Table 7.  Conversions between floating types.  */
  feq (__aeabi_d2f (dtwo), ftwo);
  deq (__aeabi_f2d (fminus_four), dminus_four);

  /* Table 8.  Integer to floating-point conversions.  */
  deq (__aeabi_i2d (-1), dminus_one);
  deq (__aeabi_ui2d (2), dtwo);
  deq (__aeabi_l2d (-1), dminus_one);
  deq (__aeabi_ul2d (2ULL), dtwo);
  feq (__aeabi_i2f (-1), fminus_one);
  feq (__aeabi_ui2f (2), ftwo);
  feq (__aeabi_l2f (-1), fminus_one);
  feq (__aeabi_ul2f (2ULL), ftwo);

  /* Table 9.  Long long functions.  */
  leq (__aeabi_lmul (4LL, -1LL), -4LL);
  leq (__aeabi_llsl (2LL, 1), 4LL);
  leq (__aeabi_llsr (-1LL, 63), 1);
  leq (__aeabi_lasr (-1LL, 63), -1);

  result = lcmp_results;
  for (i = 0; i < NUM_CMP_VALUES; i++)
    for (j = 0; j < NUM_CMP_VALUES; j++)
      for (k = 0; k < NUM_CMP_VALUES; k++)
	for (n = 0; n < NUM_CMP_VALUES; n++)
	  {
	    ieq (signof (__aeabi_lcmp
			  (((long long)cmp_val[i] << 32) | cmp_val[k],
			   ((long long)cmp_val[j] << 32) | cmp_val[n])),
			   *result);
	    result++;
	  }
  result = ulcmp_results;
  for (i = 0; i < NUM_CMP_VALUES; i++)
    for (j = 0; j < NUM_CMP_VALUES; j++)
      for (k = 0; k < NUM_CMP_VALUES; k++)
	for (n = 0; n < NUM_CMP_VALUES; n++)
	  {
	    ieq (signof (__aeabi_ulcmp
			  (((long long)cmp_val[i] << 32) | cmp_val[k],
			   ((long long)cmp_val[j] << 32) | cmp_val[n])),
			   *result);
	    result++;
	  }

  ieq (__aeabi_idiv (-550, 11), -50);
  ueq (__aeabi_uidiv (4000000000U, 1000000U), 4000U);

  for (i = 0; i < 256; i++)
    bytes[i] = i;

#ifdef __ARMEB__
  ieq (__aeabi_uread4 (bytes + 1), 0x01020304U);
  leq (__aeabi_uread8 (bytes + 3), 0x030405060708090aLL);
  ieq (__aeabi_uwrite4 (0x66778899U, bytes + 5), 0x66778899U);
  leq (__aeabi_uwrite8 (0x2030405060708090LL, bytes + 15),
       0x2030405060708090LL);
#else
  ieq (__aeabi_uread4 (bytes + 1), 0x04030201U);
  leq (__aeabi_uread8 (bytes + 3), 0x0a09080706050403LL);
  ieq (__aeabi_uwrite4 (0x99887766U, bytes + 5), 0x99887766U);
  leq (__aeabi_uwrite8 (0x9080706050403020LL, bytes + 15),
       0x9080706050403020LL);
#endif

  for (i = 0; i < 4; i++)
    ieq (bytes[5 + i], (6 + i) * 0x11);

  for (i = 0; i < 8; i++)
    ieq (bytes[15 + i], (2 + i) * 0x10);

  exit (0);		
}
