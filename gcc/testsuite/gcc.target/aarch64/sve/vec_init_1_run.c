/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "vec_init_1.c"

#define TEST_INIT_VECTOR(TYPE, VAL)		\
  {						\
  TYPE r[NUM_ELEMS (TYPE)];			\
  dup_##TYPE (r, VAL);				\
  for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
    if (r[i] != VAL)				\
      __builtin_abort ();			\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_INIT_VECTOR (int8_t, 0x2a);
  TEST_INIT_VECTOR (int16_t, 0x3976);
  TEST_INIT_VECTOR (int32_t, 0x31232976);
  TEST_INIT_VECTOR (int64_t, 0x9489363731232976LL);

  TEST_INIT_VECTOR (_Float16, -0x1.fp10);
  TEST_INIT_VECTOR (float, -0x1.fe02p10);
  TEST_INIT_VECTOR (double, 0x1.fe02eeeee1p10);

  return 0;
}
