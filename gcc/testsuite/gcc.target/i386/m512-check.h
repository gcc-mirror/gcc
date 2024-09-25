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

typedef union
{
  __m512i x;
  unsigned char a[64];
} union512i_ub;
          
typedef union
{
 __m512i x;
 unsigned short a[32];
 } union512i_uw;
                      
typedef union
{
 __m512i x;
 unsigned int a[16];
} union512i_ud;
                  
typedef union
{
 __m512i x;
 unsigned long long a[8];
} union512i_uq;

typedef union
{
  __m512h x;
  _Float16 a[32];
} union512h;

typedef union
{
  __m512bh x;
  unsigned short a[32];
} union512bf16_uw;

typedef union
{
  __m512bh x;
  __bf16 a[32];
} union512bf16_bf;

typedef union
{
  __m128h x;
  _Float16 a[8];
} union128h;

typedef union
{
  __m256h x;
  _Float16 a[16];
} union256h;

typedef union
{
  __m128bh x;
  unsigned short a[8];
} union128bf16_uw;

typedef union
{
  __m256bh x;
  unsigned short a[16];
} union256bf16_uw;

typedef union
{
  __m128bh x;
  __bf16 a[8];
} union128bf16_bf;

typedef union
{
  __m256bh x;
  __bf16 a[16];
} union256bf16_bf;

#define CHECK_ROUGH_EXP(UNION_TYPE, VALUE_TYPE, FMT)		\
static int							\
__attribute__((noinline, unused))				\
check_rough_##UNION_TYPE (UNION_TYPE u, const VALUE_TYPE *v,	\
			  VALUE_TYPE eps)			\
{								\
  int i;							\
  int err = 0;							\
								\
  for (i = 0; i < ARRAY_SIZE (u.a); i++)			\
    {								\
      /* We can have have v[i] == 0 == u.a[i]  for some i,	\
         when we test zero-masking.  */				\
      if (v[i] == 0.0 && u.a[i] == 0.0)				\
	continue;						\
      if (v[i] == 0.0 && u.a[i] != 0.0)				\
	{							\
	  err++;						\
	  PRINTF ("%i: " FMT " != " FMT "\n",			\
		  i, v[i], u.a[i]);				\
	}							\
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

#ifndef ESP_FLOAT16
#define ESP_FLOAT16 0.27
#endif

CHECK_ROUGH_EXP (union256, float, "%f")
CHECK_ROUGH_EXP (union256d, double, "%f")
CHECK_ROUGH_EXP (union128, float, "%f")
CHECK_ROUGH_EXP (union128d, double, "%f")

#ifndef AVX512F_LEN
CHECK_EXP (union512i_b, char, "%d")
CHECK_EXP (union512i_w, short, "%d")
CHECK_EXP (union512i_d, int, "0x%x")
CHECK_EXP (union512i_q, long long, "0x%llx")
CHECK_EXP (union512, float, "%f")
CHECK_EXP (union512d, double, "%f")
CHECK_EXP (union512i_ub, unsigned char, "%d")
CHECK_EXP (union512i_uw, unsigned short, "%d")
CHECK_EXP (union512i_ud, unsigned int, "0x%x")
CHECK_EXP (union512i_uq, unsigned long long, "0x%llx")
     
CHECK_FP_EXP (union512, float, ESP_FLOAT, "%f")
CHECK_FP_EXP (union512d, double, ESP_DOUBLE, "%f")

CHECK_ROUGH_EXP (union512, float, "%f")
CHECK_ROUGH_EXP (union512d, double, "%f")

#if defined(AVX512FP16)
CHECK_EXP (union512h, _Float16, "%f")
CHECK_FP_EXP (union512h, _Float16, ESP_FLOAT16, "%f")
CHECK_ROUGH_EXP (union512h, _Float16, "%f")
#endif
#endif

#if defined(AVX512FP16)
CHECK_EXP (union128h, _Float16, "%f")
CHECK_EXP (union256h, _Float16, "%f")

CHECK_FP_EXP (union128h, _Float16, ESP_FLOAT16, "%f")
CHECK_FP_EXP (union256h, _Float16, ESP_FLOAT16, "%f")

CHECK_ROUGH_EXP (union128h, _Float16, "%f")
CHECK_ROUGH_EXP (union256h, _Float16, "%f")
#endif

#if defined(AVX512BF16)
CHECK_EXP (union512bf16_uw, unsigned short, "%d")
CHECK_EXP (union512bf16_bf, __bf16, "%f")
#endif

#if defined(AVX512BF16)
CHECK_EXP (union128bf16_uw, unsigned short, "%d")
CHECK_EXP (union256bf16_uw, unsigned short, "%d")
CHECK_EXP (union128bf16_bf, __bf16, "%f")
CHECK_EXP (union256bf16_bf, __bf16, "%f")
#endif
