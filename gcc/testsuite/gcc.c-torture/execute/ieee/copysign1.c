#include <string.h>
#include <stdlib.h>

#define TEST(TYPE, EXT)						\
TYPE c##EXT (TYPE x, TYPE y)					\
{								\
  return __builtin_copysign##EXT (x, y);			\
}								\
								\
struct D##EXT { TYPE x, y, z; };				\
								\
static const struct D##EXT T##EXT[] = {				\
  { 1.0, 2.0, 1.0 },						\
  { 1.0, -2.0, -1.0 },						\
  { -1.0, -2.0, -1.0 },						\
  { 0.0, -2.0, -0.0 },						\
  { -0.0, -2.0, -0.0 },						\
  { -0.0, 2.0, 0.0 },						\
  { __builtin_inf##EXT (), -0.0, -__builtin_inf##EXT () },	\
  { -__builtin_nan##EXT (""), __builtin_inf##EXT (),		\
    __builtin_nan##EXT ("") }					\
};								\
								\
void test##EXT (void)						\
{								\
  int i, n = sizeof (T##EXT) / sizeof (T##EXT[0]);		\
  TYPE r;							\
  /* Make sure to avoid comparing unused bits in the type.  */	\
  memset (&r, 0, sizeof r);					\
  for (i = 0; i < n; ++i)					\
    {								\
      r = c##EXT (T##EXT[i].x, T##EXT[i].y);			\
      if (memcmp (&r, &T##EXT[i].z, sizeof r) != 0)		\
	abort ();						\
    }								\
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
