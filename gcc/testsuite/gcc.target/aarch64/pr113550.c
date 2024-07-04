/* { dg-options "-O" } */
/* { dg-do run } */

#pragma GCC push_options
#pragma GCC target "+ls64"
#pragma GCC aarch64 "arm_acle.h"
#pragma GCC pop_options

#define DEF_FUNCTION(NAME, ARGS)		\
  __attribute__((noipa))			\
  __arm_data512_t				\
  NAME ARGS					\
  {						\
    return *ptr;				\
  }

DEF_FUNCTION (f0, (__arm_data512_t *ptr))
DEF_FUNCTION (f1, (int x0, __arm_data512_t *ptr))
DEF_FUNCTION (f2, (int x0, int x1, __arm_data512_t *ptr))
DEF_FUNCTION (f3, (int x0, int x1, int x2, __arm_data512_t *ptr))
DEF_FUNCTION (f4, (int x0, int x1, int x2, int x3, __arm_data512_t *ptr))
DEF_FUNCTION (f5, (int x0, int x1, int x2, int x3, int x4,
		   __arm_data512_t *ptr))
DEF_FUNCTION (f6, (int x0, int x1, int x2, int x3, int x4, int x5,
		   __arm_data512_t *ptr))
DEF_FUNCTION (f7, (int x0, int x1, int x2, int x3, int x4, int x5, int x6,
		   __arm_data512_t *ptr))

int
main (void)
{
  __arm_data512_t x = { 0, 10, 20, 30, 40, 50, 60, 70 };
  __arm_data512_t res[8] =
  {
    f0 (&x),
    f1 (0, &x),
    f2 (0, 1, &x),
    f3 (0, 1, 2, &x),
    f4 (0, 1, 2, 3, &x),
    f5 (0, 1, 2, 3, 4, &x),
    f6 (0, 1, 2, 3, 4, 5, &x),
    f7 (0, 1, 2, 3, 4, 5, 6, &x)
  };
  for (int i = 0; i < 8; ++i)
    if (__builtin_memcmp (&x, &res[i], sizeof (x)) != 0)
      __builtin_abort ();
  return 0;
}
