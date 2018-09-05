/* Test the vabsd_s64 intrinsic.  */

/* { dg-do run } */
/* { dg-options "--save-temps -O2" } */

#include <arm_neon.h>
#include <limits.h>

extern void abort (void);

#define force_simd(V1)   asm volatile ("mov %d0, %1.d[0]"       \
           : "=w"(V1)                                           \
           : "w"(V1)                                            \
           : /* No clobbers */);

#define RUN_TEST(test, answ)   \
{                                      \
  force_simd (test);                   \
  force_simd (answ);                   \
  int64_t res = vabsd_s64 (test);      \
  force_simd (res);                    \
  if (res != answ)                     \
    abort ();                          \
}

int64_t input[] = {INT64_MAX, 10, 0, -10, INT64_MIN + 1, INT64_MIN};
int64_t expected[] = {INT64_MAX, 10, 0, 10, INT64_MAX, INT64_MIN};

int main (void)
{
  RUN_TEST (input[0], expected[0]);
  RUN_TEST (input[1], expected[1]);
  RUN_TEST (input[2], expected[2]);
  RUN_TEST (input[3], expected[3]);
  RUN_TEST (input[4], expected[4]);
  RUN_TEST (input[5], expected[5]);

  return 0;
}
