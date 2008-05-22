#include <stdio.h>
#include <emmintrin.h>

typedef union
{
  __m128i x;
  char a[16];
} union128i_b;

typedef union
{
  __m128i x;
  short a[8];
} union128i_w;

typedef union
{
  __m128i x;
  int a[4];
} union128i_d;

typedef union
{
  __m128i x;
  long long a[2];
} union128i_q;

typedef union
{
  __m128  x;
  float a[4];
} union128;

typedef union
{
  __m128d x;
  double a[2];
} union128d;

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(A) (sizeof (A) / sizeof ((A)[0]))
#endif

#ifdef DEBUG
#define PRINTF printf
#else
#define PRINTF(...)	
#endif

#define CHECK_EXP(UINON_TYPE, VALUE_TYPE, FMT)		\
static int						\
__attribute__((noinline, unused))			\
check_##UINON_TYPE (UINON_TYPE u, const VALUE_TYPE *v)	\
{							\
  int i;						\
  int err = 0;						\
							\
  for (i = 0; i < ARRAY_SIZE (u.a); i++)		\
    if (u.a[i] != v[i])					\
      {							\
	err++;						\
	PRINTF ("%i: " FMT " != " FMT "\n",		\
		i, v[i], u.a[i]);			\
      }							\
  return err;						\
}

CHECK_EXP (union128i_b, char, "%d")
CHECK_EXP (union128i_w, short, "%d")
CHECK_EXP (union128i_d, int, "0x%x")
CHECK_EXP (union128i_q, long long, "0x%llx")
CHECK_EXP (union128, float, "%f")
CHECK_EXP (union128d, double, "%f")
