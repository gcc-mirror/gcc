/* This file is used to reduce a number of runtime tests for AVX512F
   instructions.  Idea is to create one file per instruction -
   avx512f-insn-2.c - using defines from this file instead of intrinsic
   name, vector length etc.  Then dg-options are set with appropriate
   -Dwhatever options in that .c file producing tests for specific
   length.  */

#if defined (AVX512F)
#include "avx512f-check.h"
#elif defined (AVX512ER)
#include "avx512er-check.h"
#elif defined (AVX512CD)
#include "avx512cd-check.h"
#endif

/* Macros expansion.  */
#define CONCAT(a,b,c) a ## b ## c
#define EVAL(a,b,c) CONCAT(a,b,c)

/* Value to be written into destination.
   We have one value for all types so it must be small enough
   to fit into signed char.  */
#define DEFAULT_VALUE 117

#define MAKE_MASK_MERGE(NAME, TYPE)				      \
static void							      \
__attribute__((noinline, unused))				      \
merge_masking_##NAME (TYPE *arr, unsigned long long mask, int size)   \
{								      \
  int i;							      \
  for (i = 0; i < size; i++)					      \
    {								      \
      arr[i] = (mask & (1LL << i)) ? arr[i] : DEFAULT_VALUE;	      \
    }								      \
}

MAKE_MASK_MERGE(i_b, char)
MAKE_MASK_MERGE(i_w, short)
MAKE_MASK_MERGE(i_d, int)
MAKE_MASK_MERGE(i_q, long long)
MAKE_MASK_MERGE(, float)
MAKE_MASK_MERGE(d, double)
MAKE_MASK_MERGE(i_ub, unsigned char)
MAKE_MASK_MERGE(i_uw, unsigned short)
MAKE_MASK_MERGE(i_ud, unsigned int)
MAKE_MASK_MERGE(i_uq, unsigned long long)

#define MASK_MERGE(TYPE) merge_masking_##TYPE

#define MAKE_MASK_ZERO(NAME, TYPE)				      \
static void							      \
__attribute__((noinline, unused))				      \
zero_masking_##NAME (TYPE *arr, unsigned long long mask, int size)    \
{								      \
  int i;							      \
  for (i = 0; i < size; i++)					      \
    {								      \
      arr[i] = (mask & (1LL << i)) ? arr[i] : 0;		      \
    }								      \
}

MAKE_MASK_ZERO(i_b, char)
MAKE_MASK_ZERO(i_w, short)
MAKE_MASK_ZERO(i_d, int)
MAKE_MASK_ZERO(i_q, long long)
MAKE_MASK_ZERO(, float)
MAKE_MASK_ZERO(d, double)
MAKE_MASK_ZERO(i_ub, unsigned char)
MAKE_MASK_ZERO(i_uw, unsigned short)
MAKE_MASK_ZERO(i_ud, unsigned int)
MAKE_MASK_ZERO(i_uq, unsigned long long)


#define MASK_ZERO(TYPE) zero_masking_##TYPE

/* Intrinsic being tested.  */
#define INTRINSIC(NAME) EVAL(_mm, AVX512F_LEN, NAME)
/* Unions used for testing (for example union512d, union256d etc.).  */
#define UNION_TYPE(SIZE, NAME) EVAL(union, SIZE, NAME)
/* Corresponding union check.  */
#define UNION_CHECK(SIZE, NAME) EVAL(check_union, SIZE, NAME)
/* Corresponding fp union check.  */
#define UNION_FP_CHECK(SIZE, NAME) EVAL(check_fp_union, SIZE, NAME)
/* Corresponding rough union check.  */
#define UNION_ROUGH_CHECK(SIZE, NAME) \
  EVAL(check_rough_union, SIZE, NAME)
/* Function which tests intrinsic for given length.  */
#define TEST EVAL(test_, AVX512F_LEN,)
/* Function which calculates result.  */
#define CALC EVAL(calc_, AVX512F_LEN,)

#define AVX512F_LEN 512
#define AVX512F_LEN_HALF 256

void test_512 ();

#if defined (AVX512F)
void
avx512f_test (void) { test_512 (); }
#elif defined (AVX512CD)
void
avx512cd_test (void) { test_512 (); }
#elif defined (AVX512ER)
void
avx512er_test (void) { test_512 (); }
#endif
