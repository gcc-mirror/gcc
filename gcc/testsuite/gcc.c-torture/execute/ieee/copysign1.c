#include <string.h>
#include <stdlib.h>
#include <float.h>

#define fpsizeoff	sizeof(float)
#define fpsizeof	sizeof(double)
#define fpsizeofl	sizeof(long double)

/* Work around the fact that with the Intel double-extended precision,
   we've got a 10 byte type stuffed into some amount of padding.  And
   the fact that -ffloat-store is going to stuff this value temporarily
   into some bit of stack frame that we've no control over and can't zero.  */
#if LDBL_MANT_DIG == 64
# if defined(__i386__) || defined(__x86_64__) || defined (__ia64__)
#  undef fpsizeofl
#  define fpsizeofl	10
# endif
#endif

/* Work around the fact that the sign of the second double in the IBM
   double-double format is not strictly specified when it contains a zero.
   For instance, -0.0L can be represented with either (-0.0, +0.0) or
   (-0.0, -0.0).  The former is what we'll get from the compiler when it
   builds constants; the later is what we'll get from the negation operator
   at runtime.  */
/* ??? This hack only works for big-endian, which is fortunately true for
   all of AIX, Darwin, and Irix.  */
#if LDBL_MANT_DIG == 106
# undef fpsizeofl
# define fpsizeofl	sizeof(double)
#endif


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
  for (i = 0; i < n; ++i)					\
    {								\
      r = c##EXT (T##EXT[i].x, T##EXT[i].y);			\
      if (memcmp (&r, &T##EXT[i].z, fpsizeof##EXT) != 0)	\
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
