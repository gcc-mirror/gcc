/* Make sure that vector comaprison results are not unnecessarily ANDed
   with vectors of 1.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define COUNT1(X) if (X) count += 1
#define COUNT2(X) if (X) count -= 1
#define COUNT3(X) count += (X)
#define COUNT4(X) count -= (X)

#define COND1(X) (X)
#define COND2(X) ((X) ? 1 : 0)
#define COND3(X) ((X) ? -1 : 0)
#define COND4(X) ((X) ? 0 : 1)
#define COND5(X) ((X) ? 0 : -1)

#define TEST_LT(X, Y) ((X) < (Y))
#define TEST_LE(X, Y) ((X) <= (Y))
#define TEST_GT(X, Y) ((X) > (Y))
#define TEST_GE(X, Y) ((X) >= (Y))
#define TEST_EQ(X, Y) ((X) == (Y))
#define TEST_NE(X, Y) ((X) != (Y))

#define COUNT_LOOP(ID, TYPE, CMP_ARRAY, TEST, COUNT) \
  TYPE \
  reduc_##ID (__typeof__ (CMP_ARRAY[0]) x) \
  { \
    TYPE count = 0; \
    for (unsigned int i = 0; i < 1024; ++i) \
      COUNT (TEST (CMP_ARRAY[i], x)); \
    return count; \
  }

#define COND_LOOP(ID, ARRAY, CMP_ARRAY, TEST, COND) \
  void \
  plus_##ID (__typeof__ (CMP_ARRAY[0]) x) \
  { \
    for (unsigned int i = 0; i < 1024; ++i) \
      ARRAY[i] += COND (TEST (CMP_ARRAY[i], x)); \
  } \
  void \
  plusc_##ID (void) \
  { \
    for (unsigned int i = 0; i < 1024; ++i) \
      ARRAY[i] += COND (TEST (CMP_ARRAY[i], 10)); \
  } \
  void \
  minus_##ID (__typeof__ (CMP_ARRAY[0]) x) \
  { \
    for (unsigned int i = 0; i < 1024; ++i) \
      ARRAY[i] -= COND (TEST (CMP_ARRAY[i], x)); \
  } \
  void \
  minusc_##ID (void) \
  { \
    for (unsigned int i = 0; i < 1024; ++i) \
      ARRAY[i] += COND (TEST (CMP_ARRAY[i], 1)); \
  }

#define ALL_LOOPS(ID, ARRAY, CMP_ARRAY, TEST) \
  typedef __typeof__(ARRAY[0]) ID##_type; \
  COUNT_LOOP (ID##_1, ID##_type, CMP_ARRAY, TEST, COUNT1) \
  COUNT_LOOP (ID##_2, ID##_type, CMP_ARRAY, TEST, COUNT2) \
  COUNT_LOOP (ID##_3, ID##_type, CMP_ARRAY, TEST, COUNT3) \
  COUNT_LOOP (ID##_4, ID##_type, CMP_ARRAY, TEST, COUNT4) \
  COND_LOOP (ID##_1, ARRAY, CMP_ARRAY, TEST, COND1) \
  COND_LOOP (ID##_2, ARRAY, CMP_ARRAY, TEST, COND2) \
  COND_LOOP (ID##_3, ARRAY, CMP_ARRAY, TEST, COND3) \
  COND_LOOP (ID##_4, ARRAY, CMP_ARRAY, TEST, COND4) \
  COND_LOOP (ID##_5, ARRAY, CMP_ARRAY, TEST, COND5)

signed int asi[1024] __attribute__ ((aligned (16)));
unsigned int aui[1024] __attribute__ ((aligned (16)));
signed long long asl[1024] __attribute__ ((aligned (16)));
unsigned long long aul[1024] __attribute__ ((aligned (16)));
float af[1024] __attribute__ ((aligned (16)));
double ad[1024] __attribute__ ((aligned (16)));

ALL_LOOPS (si_si, aui, asi, TEST_LT)
ALL_LOOPS (ui_si, aui, asi, TEST_LE)
ALL_LOOPS (si_ui, aui, asi, TEST_GT)
ALL_LOOPS (ui_ui, aui, asi, TEST_GE)
ALL_LOOPS (sl_sl, asl, asl, TEST_NE)
ALL_LOOPS (ul_ul, aul, aul, TEST_EQ)
ALL_LOOPS (si_f, asi, af, TEST_LE)
ALL_LOOPS (ui_f, aui, af, TEST_GT)
ALL_LOOPS (sl_d, asl, ad, TEST_GE)
ALL_LOOPS (ul_d, aul, ad, TEST_GT)

/* { dg-final { scan-assembler-not "\tand\t" } } */
/* { dg-final { scan-assembler-not "\tld\[^\t\]*\t\[wx\]" } } */
/* { dg-final { scan-assembler-not "\tst\[^\t\]*\t\[wx\]" } } */
/* { dg-final { scan-assembler "\tldr\tq" } } */
/* { dg-final { scan-assembler "\tstr\tq" } } */
