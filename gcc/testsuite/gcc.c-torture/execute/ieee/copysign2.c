#include <string.h>
#include <stdlib.h>

#define TEST(TYPE, EXT)						\
static TYPE Y##EXT[] = {					\
  2.0, -2.0, -2.0, -2.0, -2.0, 2.0, -0.0, __builtin_inf##EXT ()	\
};								\
static const TYPE Z##EXT[] = {					\
  1.0, -1.0, -1.0, -0.0, -0.0, 0.0, -__builtin_inf##EXT (),	\
  __builtin_nan##EXT ("")					\
};								\
								\
void test##EXT (void)						\
{								\
  TYPE r[8];							\
  /* Make sure to avoid comparing unused bits in the type.  */	\
  memset (r, 0, sizeof r);					\
  r[0] = __builtin_copysign##EXT (1.0, Y##EXT[0]);		\
  r[1] = __builtin_copysign##EXT (1.0, Y##EXT[1]);		\
  r[2] = __builtin_copysign##EXT (-1.0, Y##EXT[2]);		\
  r[3] = __builtin_copysign##EXT (0.0, Y##EXT[3]);		\
  r[4] = __builtin_copysign##EXT (-0.0, Y##EXT[4]);		\
  r[5] = __builtin_copysign##EXT (-0.0, Y##EXT[5]);		\
  r[6] = __builtin_copysign##EXT (__builtin_inf##EXT (), Y##EXT[6]); \
  r[7] = __builtin_copysign##EXT (-__builtin_nan##EXT (""), Y##EXT[7]); \
  if (memcmp (r, Z##EXT, sizeof r) != 0)			\
    abort ();							\
}

TEST(float, f)
TEST(double, )
TEST(long double, l)

int main()
{
  testf();
  test();
  testl();
  return 0;
}
