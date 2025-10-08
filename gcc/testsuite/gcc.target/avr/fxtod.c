/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues -fwrapv -Wno-overflow } } */

#include <stdfix.h>

#if __SIZEOF_LONG_DOUBLE__ == 8

#define NI __attribute__((noipa))

typedef long double D;

extern D ldexpl (D, int);

typedef short fract hr_t;
typedef unsigned short fract uhr_t;
typedef fract r_t;
typedef unsigned fract ur_t;

typedef short accum hk_t;
typedef unsigned short accum uhk_t;
typedef accum k_t;
typedef unsigned accum uk_t;

#define FBITuhr 8
#define FBIThr  7
#define FBITur  16
#define FBITr   15

#define FBITuhk 8
#define FBIThk  7
#define FBITuk  16
#define FBITk   15

#define VALff(S) ((2ul << (8 * sizeof (S##bits(0)) - 1)) - 1)
#define VAL80(S) (1ul << (8 * sizeof (S##bits(0)) - 1))
#define VAL00(S) 0
#define VAL01(S) 1


#define TEST_U(S, V)				\
  NI void test_##S##_##V (void)			\
  {						\
    S##_t x = S##bits (VAL##V (S));		\
    __asm ("" : "+r" (x));			\
    D d = (D) x;				\
    D z = ldexpl (VAL##V (S), - FBIT##S);	\
    if (d != z)					\
      __builtin_exit (1);			\
  }

#define TEST_S(S, V)				\
  NI void test_##S##_##V (void)			\
  {						\
    uint32_t u32 = (VAL##V (S) & VAL80 (S))	\
      ? 1u + (VAL##V (S) ^ VALff (S))		\
      : VAL##V (S);				\
    S##_t x = S##bits (VAL##V (S));		\
    __asm ("" : "+r" (x));			\
    D d = (D) x;				\
    D z = ldexpl (u32, - FBIT##S);		\
    int s = (VAL##V (S) & VAL80 (S)) != 0;	\
    if (s == 0 && d != z)			\
      __builtin_exit (2);			\
    if (s == 1 && d != -z)			\
      __builtin_exit (3);			\
  }

#define TESTS_U(S)				\
  TEST_U (S, 00)				\
  TEST_U (S, 01)				\
  TEST_U (S, ff)				\
  TEST_U (S, 80)

#define TESTS_S(S)				\
  TEST_S (S, 00)				\
  TEST_S (S, 01)				\
  TEST_S (S, ff)				\
  TEST_S (S, 80)

TESTS_U (uhr)
TESTS_U (ur)
TESTS_U (uhk)
TESTS_U (uk)

TESTS_S (hr)
TESTS_S (r)
TESTS_S (hk)
TESTS_S (k)

#define RUN(S)		\
  test_##S##_00 ();	\
  test_##S##_01 ();	\
  test_##S##_ff ();	\
  test_##S##_80 ()
  
int main (void)
{
  RUN (uhr);
  RUN (ur);
  RUN (uhk);
  RUN (uk);

  RUN (hr);
  RUN (r);
  RUN (hk);
  RUN (k);

  return 0;
}
#else
int main (void)
{
  return 0;
}
#endif
