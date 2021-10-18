/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mavx512fp16" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512fp16 } */

#define AVX512VL
#define AVX512FP16

#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK
#include "pr102327-1.c"

#define RUNCHECK_VEC_EXTRACT(U,V,S,IDX)		\
  do						\
    {						\
      S tmp = vec_extract_##V##_##IDX ((V)U.x);	\
      if (tmp != U.a[IDX])			\
	abort();				\
    }						\
  while (0)

#define RUNCHECK_VEC_SET(UTYPE,U,V,S,IDX,NUM)		\
  do							\
    {							\
      S tmp = 3.0f;					\
      UTYPE res;					\
      res.x = vec_set_##V##_##IDX ((V)U.x, tmp);	\
      for (int i = 0; i != NUM; i++)			\
	if (i == IDX)					\
	  {						\
	    if (res.a[i] != tmp)			\
	      abort ();					\
	  }						\
	else if (res.a[i] != U.a[i])			\
	  abort();					\
    }							\
  while (0)

void
test_256 (void)
{
  union512h g1;
  union256h t1;
  union128h x1;
  int sign = 1;

  int i = 0;
  for (i = 0; i < 32; i++)
    {
      g1.a[i] = 56.78 * (i - 30) * sign;
      sign = -sign;
    }

  for (i = 0; i != 16; i++)
    {
      t1.a[i] = 90.12 * (i + 40) * sign;
      sign = -sign;
    }

  for (i = 0; i != 8; i++)
    {
      x1.a[i] = 90.12 * (i + 40) * sign;
      sign = -sign;
    }

  RUNCHECK_VEC_EXTRACT (x1, v8hf, _Float16, 4);
  RUNCHECK_VEC_EXTRACT (t1, v16hf, _Float16, 3);
  RUNCHECK_VEC_EXTRACT (t1, v16hf, _Float16, 8);
  RUNCHECK_VEC_EXTRACT (t1, v16hf, _Float16, 15);
  RUNCHECK_VEC_EXTRACT (g1, v32hf, _Float16, 5);
  RUNCHECK_VEC_EXTRACT (g1, v32hf, _Float16, 8);
  RUNCHECK_VEC_EXTRACT (g1, v32hf, _Float16, 14);
  RUNCHECK_VEC_EXTRACT (g1, v32hf, _Float16, 16);
  RUNCHECK_VEC_EXTRACT (g1, v32hf, _Float16, 24);
  RUNCHECK_VEC_EXTRACT (g1, v32hf, _Float16, 28);

  RUNCHECK_VEC_SET (union128h, x1, v8hf, _Float16, 4, 8);
  RUNCHECK_VEC_SET (union256h, t1, v16hf, _Float16, 3, 16);
  RUNCHECK_VEC_SET (union256h, t1, v16hf, _Float16, 8, 16);
  RUNCHECK_VEC_SET (union256h, t1, v16hf, _Float16, 15, 16);
  RUNCHECK_VEC_SET (union512h, g1, v32hf, _Float16, 5, 32);
  RUNCHECK_VEC_SET (union512h, g1, v32hf, _Float16, 8, 32);
  RUNCHECK_VEC_SET (union512h, g1, v32hf, _Float16, 14, 32);
  RUNCHECK_VEC_SET (union512h, g1, v32hf, _Float16, 16, 32);
  RUNCHECK_VEC_SET (union512h, g1, v32hf, _Float16, 24, 32);
  RUNCHECK_VEC_SET (union512h, g1, v32hf, _Float16, 28, 32);
}

void
test_128()
{
}
