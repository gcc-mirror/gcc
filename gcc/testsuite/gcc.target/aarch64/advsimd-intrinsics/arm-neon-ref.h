/* This file defines helper operations shared by all the tests.  */

#ifndef _ARM_NEON_REF_H_
#define _ARM_NEON_REF_H_

#include <stdio.h>
#include <inttypes.h>

/* helper type, to help write floating point results in integer form.  */
typedef uint8_t hmfloat8_t;
typedef uint16_t hfloat16_t;
typedef uint32_t hfloat32_t;
typedef uint64_t hfloat64_t;

typedef uint16_t hbfloat16_t;

extern void abort(void);
extern void *memset(void *, int, size_t);
extern void *memcpy(void *, const void *, size_t);
extern size_t strlen(const char *);

/* Helper macro to select FP16 tests.  */
#if (defined (__ARM_FP16_FORMAT_IEEE) \
     || defined (__ARM_FP16_FORMAT_ALTERNATIVE))
#define FP16_SUPPORTED (1)
#else
#undef FP16_SUPPORTED
#endif

/* Various string construction helpers.  */

/*
  The most useful at user-level are VECT_VAR and VECT_VAR_DECL, which
   construct variable names or declarations, such as:
   VECT_VAR(expected, int, 16, 4) -> expected_int16x4
   VECT_VAR_DECL(expected, int, 16, 4) -> int16x4_t expected_int16x4
*/
/* Some instructions don't exist on ARM.
   Use this macro to guard against them.  */
#ifdef __aarch64__
#define AARCH64_ONLY(X) X
#define MFLOAT8_SUPPORTED 1
#else
#define AARCH64_ONLY(X)
#define MFLOAT8_SUPPORTED 0
#endif

#if MFLOAT8_SUPPORTED
#define MFLOAT8_ONLY(X) X
#define MFLOAT8(X) (((union { uint8_t x; mfloat8_t y; }) { X }).y)
#define CONVERT(T, X) \
  ((T) _Generic ((T){}, mfloat8_t: MFLOAT8(X), default: X))
#else
#define MFLOAT8_ONLY(X)
#define CONVERT(T, X) ((T) X)
#endif

#define BITEQUAL(X, Y) (__builtin_memcmp (&X, &Y, sizeof(X)) == 0)

#define xSTR(X) #X
#define STR(X) xSTR(X)

#define xNAME1(V,T) V ## _ ##  T
#define xNAME(V,T) xNAME1(V,T)

/* VAR(foo,int,16) -> foo_int16 */
#define VAR(V,T,W) xNAME(V,T##W)
/* VAR_DECL(foo,int,16) -> int16_t foo_int16 */
#define VAR_DECL(V, T, W) T##W##_t VAR(V,T,W)

/* VECT_NAME(int,16,4) ->  int16x4 */
#define VECT_NAME(T, W, N) T##W##x##N
/* VECT_ARRAY_NAME(int,16,4,2) -> int16x4x2 */
#define VECT_ARRAY_NAME(T, W, N, L) T##W##x##N##x##L
/* VECT_TYPE(int,16,4) -> int16x4_t */
#define VECT_TYPE(T, W, N) xNAME(VECT_NAME(T,W,N),t)
/* VECT_ARRAY_TYPE(int,16,4,2) -> int16x4x2_t */
#define VECT_ARRAY_TYPE(T, W, N, L) xNAME(VECT_ARRAY_NAME(T,W,N,L),t)

/* VECT_VAR(foo,int,16,4) -> foo_int16x4 */
#define VECT_VAR(V,T,W,N) xNAME(V,VECT_NAME(T,W,N))
/* VECT_VAR_DECL(foo,int,16,4) -> int16_t foo_int16x4 */
#define VECT_VAR_DECL(V, T, W, N) T##W##_t VECT_VAR(V,T,W,N)

/* Array declarations.  */
/* ARRAY(foo,int,16,4) -> int16_t foo_int16x4[4] */
#define ARRAY(V, T, W, N) VECT_VAR_DECL(V,T,W,N)[N]

/* Arrays of vectors.  */
/* VECT_ARRAY_VAR(foo,int,16,4,2) -> foo_int16x4x2 */
#define VECT_ARRAY_VAR(V,T,W,N,L) xNAME(V,VECT_ARRAY_NAME(T,W,N,L))
/* VECT_ARRAY(foo,int,16,4,2) -> int16_t foo_int16x4x2[4*2] */
#define VECT_ARRAY(V, T, W, N, L) T##W##_t VECT_ARRAY_VAR(V,T,W,N,L)[N*L]

/* Check results vs expected values. Operates on one vector.  */
#define CHECK(MSG,T,W,N,FMT,EXPECTED,COMMENT)				\
  {									\
    int i;								\
    for(i=0; i<N ; i++)							\
      {									\
	if (VECT_VAR(result, T, W, N)[i] !=				\
	    VECT_VAR(EXPECTED, T, W, N)[i]) {				\
	  fprintf(stderr,						\
		  "ERROR in %s (%s line %d in buffer '%s') at type %s "	\
		  "index %d: got 0x%" FMT " != 0x%" FMT " %s\n",	\
		  MSG, __FILE__, __LINE__,				\
		  STR(EXPECTED),					\
		  STR(VECT_NAME(T, W, N)),				\
		  i,							\
		  VECT_VAR(result, T, W, N)[i],				\
		  VECT_VAR(EXPECTED, T, W, N)[i],			\
		  strlen(COMMENT) > 0 ? COMMENT : "");			\
	  abort();							\
	}								\
      }									\
    fprintf(stderr, "CHECKED %s %s\n", STR(VECT_TYPE(T, W, N)), MSG);	\
  }

/* Floating-point variant.  */
#define CHECK_FP(MSG,T,W,N,FMT,EXPECTED,COMMENT)			\
  {									\
    int i;								\
    for(i=0; i<N ; i++)							\
      {									\
	union fp_operand {						\
	  uint##W##_t i;						\
	  T##W##_t f;							\
	} tmp_res, tmp_exp;						\
	tmp_res.f = VECT_VAR(result, T, W, N)[i];			\
	tmp_exp.i = VECT_VAR(EXPECTED, h##T, W, N)[i];			\
	if (tmp_res.i != tmp_exp.i) {					\
	  fprintf(stderr,						\
		  "ERROR in %s (%s line %d in buffer '%s') at type %s "	\
		  "index %d: got 0x%" FMT " != 0x%" FMT " %s\n",	\
		  MSG, __FILE__, __LINE__,				\
		  STR(EXPECTED),					\
		  STR(VECT_NAME(T, W, N)),				\
		  i,							\
		  tmp_res.i,						\
		  tmp_exp.i,						\
		  strlen(COMMENT) > 0 ? COMMENT : "");			\
	  abort();							\
	}								\
      }									\
    fprintf(stderr, "CHECKED %s %s\n", STR(VECT_TYPE(T, W, N)), MSG);	\
  }

/* poly variant.  */
#define CHECK_POLY(MSG,T,W,N,FMT,EXPECTED,COMMENT)			\
  {									\
    int i;								\
    for(i=0; i<N ; i++)							\
      {									\
	union poly_operand {						\
	  uint##W##_t i;						\
	  poly##W##_t p;						\
	} tmp_res, tmp_exp;						\
	tmp_res.p = VECT_VAR(result, T, W, N)[i];			\
	tmp_exp.i = VECT_VAR(EXPECTED, T, W, N)[i];			\
	if (tmp_res.i != tmp_exp.i) {					\
	  fprintf(stderr,						\
		  "ERROR in %s (%s line %d in buffer '%s') at type %s "	\
		  "index %d: got 0x%" FMT " != 0x%" FMT " %s\n",	\
		  MSG, __FILE__, __LINE__,				\
		  STR(EXPECTED),					\
		  STR(VECT_NAME(T, W, N)),				\
		  i,							\
		  tmp_res.i,						\
		  tmp_exp.i,						\
		  strlen(COMMENT) > 0 ? COMMENT : "");			\
	  abort();							\
	}								\
      }									\
    fprintf(stderr, "CHECKED %s %s\n", STR(VECT_TYPE(T, W, N)), MSG);	\
  }

/* Clean buffer with a non-zero pattern to help diagnose buffer
   overflows.  */
#define CLEAN_PATTERN_8  0x33

#define CLEAN(VAR,T,W,N)						\
  memset(VECT_VAR(VAR, T, W, N),					\
	 CLEAN_PATTERN_8,						\
	 sizeof(VECT_VAR(VAR, T, W, N)));

/* Define output buffers, one of each size.  */
static ARRAY(result, int, 8, 8);
static ARRAY(result, int, 16, 4);
static ARRAY(result, int, 32, 2);
static ARRAY(result, int, 64, 1);
static ARRAY(result, uint, 8, 8);
static ARRAY(result, uint, 16, 4);
static ARRAY(result, uint, 32, 2);
static ARRAY(result, uint, 64, 1);
static ARRAY(result, poly, 8, 8);
static ARRAY(result, poly, 16, 4);
#if defined (__ARM_FEATURE_CRYPTO)
static ARRAY(result, poly, 64, 1);
#endif
#if MFLOAT8_SUPPORTED
static ARRAY(result, mfloat, 8, 8);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
static ARRAY(result, float, 16, 4);
#endif
static ARRAY(result, float, 32, 2);
#ifdef __aarch64__
static ARRAY(result, float, 64, 1);
#endif
static ARRAY(result, int, 8, 16);
static ARRAY(result, int, 16, 8);
static ARRAY(result, int, 32, 4);
static ARRAY(result, int, 64, 2);
static ARRAY(result, uint, 8, 16);
static ARRAY(result, uint, 16, 8);
static ARRAY(result, uint, 32, 4);
static ARRAY(result, uint, 64, 2);
static ARRAY(result, poly, 8, 16);
static ARRAY(result, poly, 16, 8);
#if defined (__ARM_FEATURE_CRYPTO)
static ARRAY(result, poly, 64, 2);
#endif
#if MFLOAT8_SUPPORTED
static ARRAY(result, mfloat, 8, 16);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
static ARRAY(result, float, 16, 8);
#endif
static ARRAY(result, float, 32, 4);
#ifdef __aarch64__
static ARRAY(result, float, 64, 2);
#endif

/* Declare expected results, one of each size. They are defined and
   initialized in each test file.  */
extern ARRAY(expected, int, 8, 8);
extern ARRAY(expected, int, 16, 4);
extern ARRAY(expected, int, 32, 2);
extern ARRAY(expected, int, 64, 1);
extern ARRAY(expected, uint, 8, 8);
extern ARRAY(expected, uint, 16, 4);
extern ARRAY(expected, uint, 32, 2);
extern ARRAY(expected, uint, 64, 1);
extern ARRAY(expected, poly, 8, 8);
extern ARRAY(expected, poly, 16, 4);
#if MFLOAT8_SUPPORTED
extern ARRAY(expected, hmfloat, 8, 8);
#endif
extern ARRAY(expected, hfloat, 16, 4);
extern ARRAY(expected, hfloat, 32, 2);
extern ARRAY(expected, hfloat, 64, 1);
extern ARRAY(expected, int, 8, 16);
extern ARRAY(expected, int, 16, 8);
extern ARRAY(expected, int, 32, 4);
extern ARRAY(expected, int, 64, 2);
extern ARRAY(expected, uint, 8, 16);
extern ARRAY(expected, uint, 16, 8);
extern ARRAY(expected, uint, 32, 4);
extern ARRAY(expected, uint, 64, 2);
extern ARRAY(expected, poly, 8, 16);
extern ARRAY(expected, poly, 16, 8);
#if MFLOAT8_SUPPORTED
extern ARRAY(expected, hmfloat, 8, 16);
#endif
extern ARRAY(expected, hfloat, 16, 8);
extern ARRAY(expected, hfloat, 32, 4);
extern ARRAY(expected, hfloat, 64, 2);

#define CHECK_RESULTS_NAMED_NO_FP16(test_name,EXPECTED,comment)		\
  {									\
    CHECK(test_name, int, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, int, 16, 4, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, int, 32, 2, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, int, 64, 1, PRIx64, EXPECTED, comment);		\
    CHECK(test_name, uint, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, uint, 16, 4, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, uint, 32, 2, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, uint, 64, 1, PRIx64, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 8, 8, PRIx8, EXPECTED, comment);	\
    CHECK_POLY(test_name, poly, 16, 4, PRIx16, EXPECTED, comment);	\
    MFLOAT8_ONLY(CHECK_FP(test_name, mfloat, 8, 8, PRIx8,		\
			  EXPECTED, comment);)				\
    CHECK_FP(test_name, float, 32, 2, PRIx32, EXPECTED, comment);	\
									\
    CHECK(test_name, int, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, int, 16, 8, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, int, 32, 4, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, int, 64, 2, PRIx64, EXPECTED, comment);		\
    CHECK(test_name, uint, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, uint, 16, 8, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, uint, 32, 4, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, uint, 64, 2, PRIx64, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 8, 16, PRIx8, EXPECTED, comment);	\
    CHECK_POLY(test_name, poly, 16, 8, PRIx16, EXPECTED, comment);	\
    MFLOAT8_ONLY(CHECK_FP(test_name, mfloat, 8, 16, PRIx8,		\
			  EXPECTED, comment);)				\
    CHECK_FP(test_name, float, 32, 4, PRIx32, EXPECTED, comment);	\
  }									\

/* Check results against EXPECTED.  Operates on all possible vector types.  */
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define CHECK_RESULTS_NAMED(test_name,EXPECTED,comment)			\
  {									\
    CHECK_RESULTS_NAMED_NO_FP16(test_name, EXPECTED, comment)		\
    CHECK_FP(test_name, float, 16, 4, PRIx16, EXPECTED, comment);	\
    CHECK_FP(test_name, float, 16, 8, PRIx16, EXPECTED, comment);	\
  }
#else
#define CHECK_RESULTS_NAMED(test_name,EXPECTED,comment)		\
  CHECK_RESULTS_NAMED_NO_FP16(test_name, EXPECTED, comment)
#endif

#define CHECK_RESULTS_NO_FP16(test_name,comment)			\
  CHECK_RESULTS_NAMED_NO_FP16(test_name, expected, comment)

#define CHECK_RESULTS(test_name,comment)		\
  CHECK_RESULTS_NAMED(test_name, expected, comment)


#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__

typedef union {
  struct {
    int _xxx:24;
    unsigned int FZ:1;
    unsigned int DN:1;
    unsigned int AHP:1;
    unsigned int QC:1;
    int V:1;
    int C:1;
    int Z:1;
    int N:1;
  } b;
  unsigned int word;
} _ARM_FPSCR;

#else /* __ORDER_BIG_ENDIAN__ */

typedef union {
  struct {
    int N:1;
    int Z:1;
    int C:1;
    int V:1;
    unsigned int QC:1;
    unsigned int AHP:1;
    unsigned int DN:1;
    unsigned int FZ:1;
    int _dnm:24;
  } b;
  unsigned int word;
} _ARM_FPSCR;

#endif /* __ORDER_BIG_ENDIAN__ */

#define Neon_Cumulative_Sat  __read_neon_cumulative_sat()
/* We need a fake dependency to ensure correct ordering of asm
   statements to preset the QC flag value, and Neon operators writing
   to QC. */
#define Set_Neon_Cumulative_Sat(x, depend)	\
  __set_neon_cumulative_sat((x), (depend))

#if defined(__aarch64__)
static volatile int __read_neon_cumulative_sat (void) {
    _ARM_FPSCR _afpscr_for_qc;
    asm volatile ("mrs %0,fpsr" : "=r" (_afpscr_for_qc));
    return _afpscr_for_qc.b.QC;
}
#define __set_neon_cumulative_sat(x, depend) {				\
    _ARM_FPSCR _afpscr_for_qc;						\
    asm volatile ("mrs %0,fpsr" : "=r" (_afpscr_for_qc));		\
    _afpscr_for_qc.b.QC = x;						\
    asm volatile ("msr fpsr,%1" : "=X" (depend) : "r" (_afpscr_for_qc)); \
  }
#else
static volatile int __read_neon_cumulative_sat (void) {
    _ARM_FPSCR _afpscr_for_qc;
    asm volatile ("vmrs %0,fpscr" : "=r" (_afpscr_for_qc));
    return _afpscr_for_qc.b.QC;
}

#define __set_neon_cumulative_sat(x, depend) {				\
    _ARM_FPSCR _afpscr_for_qc;						\
    asm volatile ("vmrs %0,fpscr" : "=r" (_afpscr_for_qc));		\
    _afpscr_for_qc.b.QC = x;						\
    asm volatile ("vmsr fpscr,%1" : "=X" (depend) : "r" (_afpscr_for_qc)); \
  }
#endif

/* Clean output buffers before execution.  */
static void clean_results (void)
{
  CLEAN(result, int, 8, 8);
  CLEAN(result, int, 16, 4);
  CLEAN(result, int, 32, 2);
  CLEAN(result, int, 64, 1);
  CLEAN(result, uint, 8, 8);
  CLEAN(result, uint, 16, 4);
  CLEAN(result, uint, 32, 2);
  CLEAN(result, uint, 64, 1);
  CLEAN(result, poly, 8, 8);
  CLEAN(result, poly, 16, 4);
#if defined (__ARM_FEATURE_CRYPTO)
  CLEAN(result, poly, 64, 1);
#endif
#if MFLOAT8_SUPPORTED
  CLEAN(result, mfloat, 8, 8);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CLEAN(result, float, 16, 4);
#endif
  CLEAN(result, float, 32, 2);

  CLEAN(result, int, 8, 16);
  CLEAN(result, int, 16, 8);
  CLEAN(result, int, 32, 4);
  CLEAN(result, int, 64, 2);
  CLEAN(result, uint, 8, 16);
  CLEAN(result, uint, 16, 8);
  CLEAN(result, uint, 32, 4);
  CLEAN(result, uint, 64, 2);
  CLEAN(result, poly, 8, 16);
  CLEAN(result, poly, 16, 8);
#if defined (__ARM_FEATURE_CRYPTO)
  CLEAN(result, poly, 64, 2);
#endif
#if MFLOAT8_SUPPORTED
  CLEAN(result, mfloat, 8, 16);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CLEAN(result, float, 16, 8);
#endif
  CLEAN(result, float, 32, 4);

  AARCH64_ONLY(CLEAN(result, float, 64, 2));

#if defined(__aarch64__)
  /* On AArch64, make sure to return DefaultNaN to have the same
     results as on AArch32.  */
  _ARM_FPSCR _afpscr;
  asm volatile ("mrs %0,fpcr" : "=r" (_afpscr));
  _afpscr.b.DN = 1;

  /* On AArch64, make sure to flush to zero by default, as on
     AArch32. */
  _afpscr.b.FZ = 1;

  asm volatile ("msr fpcr,%0" : : "r" (_afpscr));
#endif
}


/* Helpers to declare variables of various types.   */
#define DECL_VARIABLE(VAR, T1, W, N)		\
  VECT_TYPE(T1, W, N) VECT_VAR(VAR, T1, W, N)

#if defined (__ARM_FEATURE_CRYPTO)
#define DECL_VARIABLE_CRYPTO(VAR, T1, W, N) \
  DECL_VARIABLE(VAR, T1, W, N)
#else
#define DECL_VARIABLE_CRYPTO(VAR, T1, W, N)
#endif

/* Declare only 64 bits signed variants.  */
#define DECL_VARIABLE_64BITS_SIGNED_VARIANTS(VAR)	\
  DECL_VARIABLE(VAR, int, 8, 8);			\
  DECL_VARIABLE(VAR, int, 16, 4);			\
  DECL_VARIABLE(VAR, int, 32, 2);			\
  DECL_VARIABLE(VAR, int, 64, 1)

/* Declare only 64 bits unsigned variants.  */
#define DECL_VARIABLE_64BITS_UNSIGNED_VARIANTS(VAR)	\
  DECL_VARIABLE(VAR, uint, 8, 8);			\
  DECL_VARIABLE(VAR, uint, 16, 4);			\
  DECL_VARIABLE(VAR, uint, 32, 2);			\
  DECL_VARIABLE(VAR, uint, 64, 1)

/* Declare only 128 bits signed variants.  */
#define DECL_VARIABLE_128BITS_SIGNED_VARIANTS(VAR)	\
  DECL_VARIABLE(VAR, int, 8, 16);			\
  DECL_VARIABLE(VAR, int, 16, 8);			\
  DECL_VARIABLE(VAR, int, 32, 4);			\
  DECL_VARIABLE(VAR, int, 64, 2)

/* Declare only 128 bits unsigned variants.  */
#define DECL_VARIABLE_128BITS_UNSIGNED_VARIANTS(VAR)	\
  DECL_VARIABLE(VAR, uint, 8, 16);			\
  DECL_VARIABLE(VAR, uint, 16, 8);			\
  DECL_VARIABLE(VAR, uint, 32, 4);			\
  DECL_VARIABLE(VAR, uint, 64, 2)

/* Declare all 64 bits variants.  */
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define DECL_VARIABLE_64BITS_VARIANTS(VAR)	\
  DECL_VARIABLE_64BITS_SIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE_64BITS_UNSIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE(VAR, poly, 8, 8);		\
  DECL_VARIABLE(VAR, poly, 16, 4);		\
  DECL_VARIABLE_CRYPTO(VAR, poly, 64, 1);	\
  MFLOAT8_ONLY(DECL_VARIABLE(VAR, mfloat, 8, 8);) \
  DECL_VARIABLE(VAR, float, 16, 4);		\
  DECL_VARIABLE(VAR, float, 32, 2)
#else
#define DECL_VARIABLE_64BITS_VARIANTS(VAR)	\
  DECL_VARIABLE_64BITS_SIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE_64BITS_UNSIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE(VAR, poly, 8, 8);		\
  DECL_VARIABLE(VAR, poly, 16, 4);		\
  DECL_VARIABLE_CRYPTO(VAR, poly, 64, 1);	\
  DECL_VARIABLE(VAR, float, 32, 2)
#endif

/* Declare all 128 bits variants.  */
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define DECL_VARIABLE_128BITS_VARIANTS(VAR)	\
  DECL_VARIABLE_128BITS_SIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE_128BITS_UNSIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE(VAR, poly, 8, 16);		\
  DECL_VARIABLE(VAR, poly, 16, 8);		\
  DECL_VARIABLE_CRYPTO(VAR, poly, 64, 2);	\
  MFLOAT8_ONLY(DECL_VARIABLE(VAR, mfloat, 8, 16);) \
  DECL_VARIABLE(VAR, float, 16, 8);		\
  DECL_VARIABLE(VAR, float, 32, 4);		\
  AARCH64_ONLY(DECL_VARIABLE(VAR, float, 64, 2))
#else
#define DECL_VARIABLE_128BITS_VARIANTS(VAR)	\
  DECL_VARIABLE_128BITS_SIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE_128BITS_UNSIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE(VAR, poly, 8, 16);		\
  DECL_VARIABLE(VAR, poly, 16, 8);		\
  DECL_VARIABLE_CRYPTO(VAR, poly, 64, 2);	\
  MFLOAT8_ONLY(DECL_VARIABLE(VAR, mfloat, 8, 16);) \
  DECL_VARIABLE(VAR, float, 32, 4);		\
  AARCH64_ONLY(DECL_VARIABLE(VAR, float, 64, 2))
#endif
/* Declare all variants.  */
#define DECL_VARIABLE_ALL_VARIANTS(VAR)		\
  DECL_VARIABLE_64BITS_VARIANTS(VAR);		\
  DECL_VARIABLE_128BITS_VARIANTS(VAR)

/* Declare all signed variants.  */
#define DECL_VARIABLE_SIGNED_VARIANTS(VAR)	\
  DECL_VARIABLE_64BITS_SIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE_128BITS_SIGNED_VARIANTS(VAR)

/* Declare all unsigned variants.  */
#define DECL_VARIABLE_UNSIGNED_VARIANTS(VAR)	\
  DECL_VARIABLE_64BITS_UNSIGNED_VARIANTS(VAR);	\
  DECL_VARIABLE_128BITS_UNSIGNED_VARIANTS(VAR)

/* Helpers to initialize vectors.  */
#define VDUP(VAR, Q, T1, T2, W, N, V)			\
  VECT_VAR(VAR, T1, W, N) = vdup##Q##_n_##T2##W(V)

#define VSET_LANE(VAR, Q, T1, T2, W, N, L, V)				\
  VECT_VAR(VAR, T1, W, N) = vset##Q##_lane_##T2##W(V,			\
						   VECT_VAR(VAR, T1, W, N), \
						   L)

/* We need to load initial values first, so rely on VLD1.  */
#define VLOAD(VAR, BUF, Q, T1, T2, W, N)				\
  VECT_VAR(VAR, T1, W, N) = vld1##Q##_##T2##W(VECT_VAR(BUF, T1, W, N))

/* Helpers to call macros with 1 constant and 5 variable
   arguments.  */
#if defined (__ARM_FEATURE_CRYPTO)
#define MACRO_CRYPTO(MACRO, VAR1, VAR2, T1, T2, T3, W, N) \
  MACRO(VAR1, VAR2, T1, T2, T3, W, N)
#else
#define MACRO_CRYPTO(MACRO, VAR1, VAR2, T1, T2, T3, W, N)
#endif

#define TEST_MACRO_64BITS_SIGNED_VARIANTS_1_5(MACRO, VAR)	\
  MACRO(VAR, , int, s, 8, 8);					\
  MACRO(VAR, , int, s, 16, 4);					\
  MACRO(VAR, , int, s, 32, 2);					\
  MACRO(VAR, , int, s, 64, 1)

#define TEST_MACRO_64BITS_UNSIGNED_VARIANTS_1_5(MACRO, VAR)	\
  MACRO(VAR, , uint, u, 8, 8);					\
  MACRO(VAR, , uint, u, 16, 4);					\
  MACRO(VAR, , uint, u, 32, 2);					\
  MACRO(VAR, , uint, u, 64, 1)

#define TEST_MACRO_128BITS_SIGNED_VARIANTS_1_5(MACRO, VAR)	\
  MACRO(VAR, q, int, s, 8, 16);					\
  MACRO(VAR, q, int, s, 16, 8);					\
  MACRO(VAR, q, int, s, 32, 4);					\
  MACRO(VAR, q, int, s, 64, 2)

#define TEST_MACRO_128BITS_UNSIGNED_VARIANTS_1_5(MACRO,VAR)	\
  MACRO(VAR, q, uint, u, 8, 16);				\
  MACRO(VAR, q, uint, u, 16, 8);				\
  MACRO(VAR, q, uint, u, 32, 4);				\
  MACRO(VAR, q, uint, u, 64, 2)

#define TEST_MACRO_64BITS_VARIANTS_1_5(MACRO, VAR)	\
  TEST_MACRO_64BITS_SIGNED_VARIANTS_1_5(MACRO, VAR);	\
  TEST_MACRO_64BITS_UNSIGNED_VARIANTS_1_5(MACRO, VAR)

#define TEST_MACRO_128BITS_VARIANTS_1_5(MACRO, VAR)	\
  TEST_MACRO_128BITS_SIGNED_VARIANTS_1_5(MACRO, VAR);	\
  TEST_MACRO_128BITS_UNSIGNED_VARIANTS_1_5(MACRO, VAR)

#define TEST_MACRO_ALL_VARIANTS_1_5(MACRO, VAR)	\
  TEST_MACRO_64BITS_VARIANTS_1_5(MACRO, VAR);	\
  TEST_MACRO_128BITS_VARIANTS_1_5(MACRO, VAR)

#define TEST_MACRO_SIGNED_VARIANTS_1_5(MACRO, VAR)	\
  TEST_MACRO_64BITS_SIGNED_VARIANTS_1_5(MACRO, VAR);	\
  TEST_MACRO_128BITS_SIGNED_VARIANTS_1_5(MACRO, VAR)

/* Helpers to call macros with 2 constant and 5 variable
   arguments.  */
#define TEST_MACRO_64BITS_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  MACRO(VAR1, VAR2, , int, s, 8, 8);					\
  MACRO(VAR1, VAR2, , int, s, 16, 4);					\
  MACRO(VAR1, VAR2, , int, s, 32, 2);					\
  MACRO(VAR1, VAR2 , , int, s, 64, 1)

#define TEST_MACRO_64BITS_UNSIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  MACRO(VAR1, VAR2, , uint, u, 8, 8);					\
  MACRO(VAR1, VAR2, , uint, u, 16, 4);					\
  MACRO(VAR1, VAR2, , uint, u, 32, 2);					\
  MACRO(VAR1, VAR2, , uint, u, 64, 1)

#define TEST_MACRO_128BITS_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  MACRO(VAR1, VAR2, q, int, s, 8, 16);					\
  MACRO(VAR1, VAR2, q, int, s, 16, 8);					\
  MACRO(VAR1, VAR2, q, int, s, 32, 4);					\
  MACRO(VAR1, VAR2, q, int, s, 64, 2)

#define TEST_MACRO_128BITS_UNSIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  MACRO(VAR1, VAR2, q, uint, u, 8, 16);					\
  MACRO(VAR1, VAR2, q, uint, u, 16, 8);					\
  MACRO(VAR1, VAR2, q, uint, u, 32, 4);					\
  MACRO(VAR1, VAR2, q, uint, u, 64, 2)

#define TEST_MACRO_64BITS_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  TEST_MACRO_64BITS_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2);	\
  TEST_MACRO_64BITS_UNSIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2);	\
  MACRO(VAR1, VAR2, , poly, p, 8, 8);				\
  MACRO(VAR1, VAR2, , poly, p, 16, 4);				\
  MACRO_CRYPTO(MACRO, VAR1, VAR2, , poly, p, 64, 1)

#define TEST_MACRO_128BITS_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  TEST_MACRO_128BITS_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2);	\
  TEST_MACRO_128BITS_UNSIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2);	\
  MACRO(VAR1, VAR2, q, poly, p, 8, 16);				\
  MACRO(VAR1, VAR2, q, poly, p, 16, 8);				\
  MACRO_CRYPTO(MACRO, VAR1, VAR2, q, poly, p, 64, 2)

#define TEST_MACRO_ALL_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  TEST_MACRO_64BITS_VARIANTS_2_5(MACRO, VAR1, VAR2);	\
  TEST_MACRO_128BITS_VARIANTS_2_5(MACRO, VAR1, VAR2)

#define TEST_MACRO_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2)	\
  TEST_MACRO_64BITS_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2);	\
  TEST_MACRO_128BITS_SIGNED_VARIANTS_2_5(MACRO, VAR1, VAR2)

#endif /* _ARM_NEON_REF_H_ */
