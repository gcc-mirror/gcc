/* Test for _Atomic in C11.  Test that compare-and-exchange is
   operating properly when operations on the same variable are carried
   out in two threads.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors -pthread -U_POSIX_C_SOURCE -D_POSIX_C_SOURCE=200809L" } */
/* { dg-additional-options "-D_XOPEN_SOURCE=600" { target *-*-solaris2* } } */
/* { dg-require-effective-target pthread } */

#include <stdint.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define ITER_COUNT 10000

static volatile _Atomic bool thread_ready;

/* Generate test code (with NAME used to name functions and variables)
   for atomic compound assignments to a variable of type LHSTYPE.  The
   variable is initialized to INIT, then PRE var POST is executed
   ITER_COUNT times in each of two threads, and the final result
   should be FINAL.  A function test_main_##NAME is generated that
   returns nonzero on failure, zero on success.  */

#define TEST_FUNCS(NAME, LHSTYPE, PRE, POST, INIT, FINAL)		\
									\
static volatile _Atomic LHSTYPE var_##NAME = (INIT);			\
									\
static void *								\
test_thread_##NAME (void *arg)						\
{									\
  thread_ready = true;							\
  for (int i = 0; i < ITER_COUNT; i++)					\
    PRE var_##NAME POST;						\
  return NULL;								\
}									\
									\
static int								\
test_main_##NAME (void)							\
{									\
  thread_ready = false;							\
  pthread_t thread_id;							\
  int pret = pthread_create (&thread_id, NULL, test_thread_##NAME,	\
			     NULL);					\
  if (pret != 0)							\
    {									\
      printf ("pthread_create failed: %d\n", pret);			\
      return 1;								\
    }									\
  while (!thread_ready)							\
    ;									\
  for (int i = 0; i < ITER_COUNT; i++)					\
    PRE var_##NAME POST;						\
  pthread_join (thread_id, NULL);					\
  if (var_##NAME != (FINAL))						\
    {									\
      printf (#NAME " failed\n");					\
      return 1;								\
    }									\
  else									\
    {									\
      printf (#NAME " passed\n");					\
      return 0;								\
    }									\
}

TEST_FUNCS (uint8_add, uint8_t, , += 1, 0, (uint8_t) 20000)
TEST_FUNCS (uint8_add_3, uint8_t, , += 3, 0, (uint8_t) 60000)
TEST_FUNCS (uint16_add, uint16_t, , += 1, 0, (uint16_t) 20000)
TEST_FUNCS (uint16_add_3, uint16_t, , += 3, 0, (uint16_t) 60000)
TEST_FUNCS (uint32_add, uint32_t, , += 1, 0, (uint32_t) 20000)
TEST_FUNCS (uint32_add_3, uint32_t, , += 3, 0, (uint32_t) 60000)
TEST_FUNCS (uint64_add, uint64_t, , += 1, 0, (uint64_t) 20000)
TEST_FUNCS (uint64_add_3, uint64_t, , += 3, 0, (uint64_t) 60000)
TEST_FUNCS (uint64_add_neg, uint64_t, , += 1, -10000, (uint64_t) 10000)
TEST_FUNCS (float_add, float, , += 1, 0, 20000)
TEST_FUNCS (double_add, double, , += 1, 0, 20000)
TEST_FUNCS (long_double_add, long double, , += 1, 0, 20000)
TEST_FUNCS (complex_float_add, _Complex float, , += 1, 0, 20000)
TEST_FUNCS (complex_double_add, _Complex double, , += 1, 0, 20000)
TEST_FUNCS (complex_long_double_add, _Complex long double, , += 1, 0, 20000)
TEST_FUNCS (uint8_postinc, uint8_t, , ++, 0, (uint8_t) 20000)
TEST_FUNCS (uint16_postinc, uint16_t, , ++, 0, (uint16_t) 20000)
TEST_FUNCS (uint32_postinc, uint32_t, , ++, 0, (uint32_t) 20000)
TEST_FUNCS (uint64_postinc, uint64_t, , ++, 0, (uint64_t) 20000)
TEST_FUNCS (uint64_postinc_neg, uint64_t, , ++, -10000, (uint64_t) 10000)
TEST_FUNCS (float_postinc, float, , ++, 0, 20000)
TEST_FUNCS (double_postinc, double, , ++, 0, 20000)
TEST_FUNCS (long_double_postinc, long double, , ++, 0, 20000)
TEST_FUNCS (uint8_preinc, uint8_t, ++, , 0, (uint8_t) 20000)
TEST_FUNCS (uint16_preinc, uint16_t, ++, , 0, (uint16_t) 20000)
TEST_FUNCS (uint32_preinc, uint32_t, ++, , 0, (uint32_t) 20000)
TEST_FUNCS (uint64_preinc, uint64_t, ++, , 0, (uint64_t) 20000)
TEST_FUNCS (uint64_preinc_neg, uint64_t, ++, , -10000, (uint64_t) 10000)
TEST_FUNCS (float_preinc, float, ++, , 0, 20000)
TEST_FUNCS (double_preinc, double, ++, , 0, 20000)
TEST_FUNCS (long_double_preinc, long double, ++, , 0, 20000)
TEST_FUNCS (uint8_sub, uint8_t, , -= 1, 0, (uint8_t) -20000)
TEST_FUNCS (uint8_sub_3, uint8_t, , -= 3, 0, (uint8_t) -60000)
TEST_FUNCS (uint16_sub, uint16_t, , -= 1, 0, (uint16_t) -20000)
TEST_FUNCS (uint16_sub_3, uint16_t, , -= 3, 0, (uint16_t) -60000)
TEST_FUNCS (uint32_sub, uint32_t, , -= 1, 0, (uint32_t) -20000)
TEST_FUNCS (uint32_sub_3, uint32_t, , -= 3, 0, (uint32_t) -60000)
TEST_FUNCS (uint64_sub, uint64_t, , -= 1, 0, (uint64_t) -20000)
TEST_FUNCS (uint64_sub_3, uint64_t, , -= 3, 0, (uint64_t) -60000)
TEST_FUNCS (uint64_sub_neg, uint64_t, , -= 1, 10000, (uint64_t) -10000)
TEST_FUNCS (float_sub, float, , -= 1, 0, -20000)
TEST_FUNCS (double_sub, double, , -= 1, 0, -20000)
TEST_FUNCS (long_double_sub, long double, , -= 1, 0, -20000)
TEST_FUNCS (complex_float_sub, _Complex float, , -= 1, 0, -20000)
TEST_FUNCS (complex_double_sub, _Complex double, , -= 1, 0, -20000)
TEST_FUNCS (complex_long_double_sub, _Complex long double, , -= 1, 0, -20000)
TEST_FUNCS (uint8_postdec, uint8_t, , --, 0, (uint8_t) -20000)
TEST_FUNCS (uint16_postdec, uint16_t, , --, 0, (uint16_t) -20000)
TEST_FUNCS (uint32_postdec, uint32_t, , --, 0, (uint32_t) -20000)
TEST_FUNCS (uint64_postdec, uint64_t, , --, 0, (uint64_t) -20000)
TEST_FUNCS (uint64_postdec_neg, uint64_t, , --, 10000, (uint64_t) -10000)
TEST_FUNCS (float_postdec, float, , --, 0, -20000)
TEST_FUNCS (double_postdec, double, , --, 0, -20000)
TEST_FUNCS (long_double_postdec, long double, , --, 0, -20000)
TEST_FUNCS (uint8_predec, uint8_t, --, , 0, (uint8_t) -20000)
TEST_FUNCS (uint16_predec, uint16_t, --, , 0, (uint16_t) -20000)
TEST_FUNCS (uint32_predec, uint32_t, --, , 0, (uint32_t) -20000)
TEST_FUNCS (uint64_predec, uint64_t, --, , 0, (uint64_t) -20000)
TEST_FUNCS (uint64_predec_neg, uint64_t, --, , 10000, (uint64_t) -10000)
TEST_FUNCS (float_predec, float, --, , 0, -20000)
TEST_FUNCS (double_predec, double, --, , 0, -20000)
TEST_FUNCS (long_double_predec, long double, --, , 0, -20000)
TEST_FUNCS (uint8_mul, uint8_t, , *= 3, 1, (uint8_t) 0x81)
TEST_FUNCS (uint16_mul, uint16_t, , *= 3, 1, (uint16_t) 0x9681)
TEST_FUNCS (uint32_mul, uint32_t, , *= 3, 1, (uint32_t) 0x62b49681U)
TEST_FUNCS (uint64_mul, uint64_t, , *= 3, 1, (uint64_t) 0xcd926beb62b49681ULL)

int
main (void)
{
  int ret = 0;
  ret |= test_main_uint8_add ();
  ret |= test_main_uint8_add_3 ();
  ret |= test_main_uint16_add ();
  ret |= test_main_uint16_add_3 ();
  ret |= test_main_uint32_add ();
  ret |= test_main_uint32_add_3 ();
  ret |= test_main_uint64_add ();
  ret |= test_main_uint64_add_3 ();
  ret |= test_main_uint64_add_neg ();
  ret |= test_main_float_add ();
  ret |= test_main_double_add ();
  ret |= test_main_long_double_add ();
  ret |= test_main_complex_float_add ();
  ret |= test_main_complex_double_add ();
  ret |= test_main_complex_long_double_add ();
  ret |= test_main_uint8_postinc ();
  ret |= test_main_uint16_postinc ();
  ret |= test_main_uint32_postinc ();
  ret |= test_main_uint64_postinc ();
  ret |= test_main_uint64_postinc_neg ();
  ret |= test_main_float_postinc ();
  ret |= test_main_double_postinc ();
  ret |= test_main_long_double_postinc ();
  ret |= test_main_uint8_preinc ();
  ret |= test_main_uint16_preinc ();
  ret |= test_main_uint32_preinc ();
  ret |= test_main_uint64_preinc ();
  ret |= test_main_uint64_preinc_neg ();
  ret |= test_main_float_preinc ();
  ret |= test_main_double_preinc ();
  ret |= test_main_long_double_preinc ();
  ret |= test_main_uint8_sub ();
  ret |= test_main_uint8_sub_3 ();
  ret |= test_main_uint16_sub ();
  ret |= test_main_uint16_sub_3 ();
  ret |= test_main_uint32_sub ();
  ret |= test_main_uint32_sub_3 ();
  ret |= test_main_uint64_sub ();
  ret |= test_main_uint64_sub_3 ();
  ret |= test_main_uint64_sub_neg ();
  ret |= test_main_float_sub ();
  ret |= test_main_double_sub ();
  ret |= test_main_long_double_sub ();
  ret |= test_main_complex_float_sub ();
  ret |= test_main_complex_double_sub ();
  ret |= test_main_complex_long_double_sub ();
  ret |= test_main_uint8_postdec ();
  ret |= test_main_uint16_postdec ();
  ret |= test_main_uint32_postdec ();
  ret |= test_main_uint64_postdec ();
  ret |= test_main_uint64_postdec_neg ();
  ret |= test_main_float_postdec ();
  ret |= test_main_double_postdec ();
  ret |= test_main_long_double_postdec ();
  ret |= test_main_uint8_predec ();
  ret |= test_main_uint16_predec ();
  ret |= test_main_uint32_predec ();
  ret |= test_main_uint64_predec ();
  ret |= test_main_uint64_predec_neg ();
  ret |= test_main_float_predec ();
  ret |= test_main_double_predec ();
  ret |= test_main_long_double_predec ();
  ret |= test_main_uint8_mul ();
  ret |= test_main_uint16_mul ();
  ret |= test_main_uint32_mul ();
  ret |= test_main_uint64_mul ();
  if (ret)
    abort ();
  else
    exit (0);
}
