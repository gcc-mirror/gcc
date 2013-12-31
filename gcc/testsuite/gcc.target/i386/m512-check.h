#include <immintrin.h>
#include "m256-check.h"

typedef union
{
  __m512i x;
  char a[64];
} union512i_b;

typedef union
{
  __m512i x;
  short a[32];
} union512i_w;

typedef union
{
  __m512i x;
  int a[16];
} union512i_d;

typedef union
{
  __m512i x;
  long long a[8];
} union512i_q;

typedef union
{
  __m512 x;
  float a[16];
} union512;

typedef union
{
  __m512d x;
  double a[8];
} union512d;

CHECK_EXP (union512i_b, char, "%d")
CHECK_EXP (union512i_w, short, "%d")
CHECK_EXP (union512i_d, int, "0x%x")
CHECK_EXP (union512i_q, long long, "0x%llx")
CHECK_EXP (union512, float, "%f")
CHECK_EXP (union512d, double, "%f")

CHECK_FP_EXP (union512, float, ESP_FLOAT, "%f")
CHECK_FP_EXP (union512d, double, ESP_DOUBLE, "%f")

#define CHECK_ROUGH_EXP(UINON_TYPE, VALUE_TYPE, FMT)		\
static int							\
__attribute__((noinline, unused))				\
check_rough_##UINON_TYPE (UINON_TYPE u, const VALUE_TYPE *v,	\
			  VALUE_TYPE eps)			\
{								\
  int i;							\
  int err = 0;							\
								\
  for (i = 0; i < ARRAY_SIZE (u.a); i++)			\
    {								\
      VALUE_TYPE rel_err = (u.a[i] - v[i]) / v[i];		\
      if (((rel_err < 0) ? -rel_err : rel_err) > eps)		\
	{							\
	  err++;						\
	  PRINTF ("%i: " FMT " != " FMT "\n",			\
		  i, v[i], u.a[i]);				\
	}							\
    }								\
  return err;							\
}

CHECK_ROUGH_EXP (union512, float, "%f")
CHECK_ROUGH_EXP (union512d, double, "%f")
