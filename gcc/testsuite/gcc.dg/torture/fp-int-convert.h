/* Test floating-point conversions.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */

#include <limits.h>
extern void abort (void);
extern void exit (int);

/* Not all platforms support TImode integers; logic as in
   gcc.dg/titype-1.c.  */
#if (defined(__LP64__) && !defined(__hppa__)) || defined(_WIN64) || defined(__SPU__)
typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#else
typedef long TItype;
typedef unsigned long UTItype;
#endif

/* TEST_I_F(I, U, F, P, M) tests conversions between the pair of
   signed and unsigned integer types I and U and the floating-point
   type F, where P is the binary precision of the floating point type
   and M is the MAX_EXP value for that type (so 2^M overflows, 2^(M-1)
   does not).  We test conversions of the values 0, 1, 0x7...f,
   0x8...0, 0xf...f.  We also test conversions of values half way
   between two representable values (rounding both ways), just above
   half way, and just below half way.  */
#define TEST_I_F(I, U, F, P, M)					\
do {								\
  TEST_I_F_VAL (I, F, (I)0, 1);					\
  TEST_I_F_VAL (I, F, (I)1, 1);					\
  TEST_I_F_VAL (I, F, (I)(((U)~(U)0) >> 1), P_OK1 (P, I));	\
  TEST_I_F_VAL (I, F, (I)(U)~(((U)~(U)0) >> 1), M_OK1 (M, I));	\
  TEST_I_F_VAL (I, F, (I)(U)~(U)0, 1);				\
  TEST_I_F_VAL (I, F, HVAL0S (P, I), P_OK (P, I));		\
  TEST_I_F_VAL (I, F, HVAL0S (P, I) + 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, HVAL0S (P, I) - 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, HVAL1S (P, I), P_OK (P, I));		\
  TEST_I_F_VAL (I, F, HVAL1S (P, I) + 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, HVAL1S (P, I) - 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, -HVAL0S (P, I), P_OK (P, I));		\
  TEST_I_F_VAL (I, F, -HVAL0S (P, I) + 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, -HVAL0S (P, I) - 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, -HVAL1S (P, I), P_OK (P, I));		\
  TEST_I_F_VAL (I, F, -HVAL1S (P, I) + 1, P_OK (P, I));		\
  TEST_I_F_VAL (I, F, -HVAL1S (P, I) - 1, P_OK (P, I));		\
  TEST_I_F_VAL (U, F, (U)0, 1);					\
  TEST_I_F_VAL (U, F, (U)1, 1);					\
  TEST_I_F_VAL (U, F, (U)(((U)~(U)0) >> 1), P_OK1 (P, U));	\
  TEST_I_F_VAL (U, F, (U)~(((U)~(U)0) >> 1), M_OK1 (M, U));	\
  TEST_I_F_VAL (U, F, (U)~(U)0, P_OK (P, U));			\
  TEST_I_F_VAL (U, F, HVAL0U (P, U), P_OK (P, U));		\
  TEST_I_F_VAL (U, F, HVAL0U (P, U) + 1, P_OK (P, U));		\
  TEST_I_F_VAL (U, F, HVAL0U (P, U) - 1, P_OK (P, U));		\
  TEST_I_F_VAL (U, F, HVAL1U (P, U), P_OK (P, U));		\
  TEST_I_F_VAL (U, F, HVAL1U (P, U) + 1, P_OK (P, U));		\
  TEST_I_F_VAL (U, F, HVAL1U (P, U) - 1, P_OK (P, U));		\
  TEST_I_F_VAL (I, F, WVAL0S (I), M_OK2 (M, U));		\
  TEST_I_F_VAL (I, F, -WVAL0S (I), M_OK2 (M, U));		\
} while (0)

#define P_OK(P, T) ((P) >= sizeof(T) * CHAR_BIT)
#define P_OK1(P, T) ((P) >= sizeof(T) * CHAR_BIT - 1)
#define M_OK1(M, T) ((M) > sizeof(T) * CHAR_BIT - 1)
#define M_OK2(M, T) ((M) > sizeof(T) * CHAR_BIT / 2 - 1)
#define HVAL0U(P, U) (U)(P_OK (P, U)					 \
			 ? (U)1						 \
			 : (((U)1 << (sizeof(U) * CHAR_BIT - 1))	 \
			    + ((U)1 << (sizeof(U) * CHAR_BIT - 1 - P))))
#define HVAL1U(P, U) (U)(P_OK (P, U)					 \
			 ? (U)1						 \
			 : (((U)1 << (sizeof(U) * CHAR_BIT - 1))	 \
			    + ((U)3 << (sizeof(U) * CHAR_BIT - 1 - P))))
#define HVAL0S(P, S) (S)(P_OK1 (P, S)					 \
			 ? (S)1						 \
			 : (((S)1 << (sizeof(S) * CHAR_BIT - 2))	 \
			    + ((S)1 << (sizeof(S) * CHAR_BIT - 2 - P))))
#define HVAL1S(P, S) (S)(P_OK1 (P, S)					 \
			 ? (S)1						 \
			 : (((S)1 << (sizeof(S) * CHAR_BIT - 2))	 \
			    + ((S)3 << (sizeof(S) * CHAR_BIT - 2 - P))))
#define WVAL0S(S) (S)((S)1 << (sizeof(S) * CHAR_BIT / 2 - 1))

#define TEST_I_F_VAL(IT, FT, VAL, PREC_OK)		\
do {							\
  static volatile IT ivin, ivout;			\
  static volatile FT fv1, fv2;				\
  ivin = (VAL);						\
  fv1 = (VAL);						\
  fv2 = ivin;						\
  ivout = fv2;						\
  if (ivin != (VAL)					\
      || ((PREC_OK) && ivout != ivin)			\
      || ((PREC_OK) && ivout != (VAL))			\
      || fv1 != (FT) (VAL) || fv2 != (FT) (VAL)		\
      || fv1 != fv2)					\
    abort ();						\
} while (0)
