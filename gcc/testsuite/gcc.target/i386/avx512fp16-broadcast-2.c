/* PR target/87767 */
/* { dg-do run } */
/* { dg-options "-O1 -mavx512fp16 -mavx512dq -mavx512vl" } */
/* { dg-require-effective-target avx512dq } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512fp16 } */

#define AVX512DQ
#define AVX512VL
#define AVX512FP16
#include "avx512f-helper.h"

#include "avx512fp16-broadcast-1.c"

#define RTEST(VTYPE, TYPE, N, OP_NAME, OP)             \
  do                                                   \
    {                                                  \
      TYPE exp[N], src[N];                             \
      VTYPE res;                                       \
      for (int i = 0; i < N; i++)                      \
       src[i] = 2.0 * i - 8.4;                         \
      res = foo_##OP_NAME##_##VTYPE (*(VTYPE*)&src[0]);        \
      for (int i = 0; i < N; i ++)                     \
       exp[i] = src[i] OP CONSTANT;                    \
      for (int j = 0; j < N; j++)                      \
       {                                               \
         if (res[j] != exp[j])                         \
           abort();                                    \
       }                                               \
    }                                                  \
  while (0)

void
test_256 (void)
{
  RTEST (v8hf, _Float16, 8, add, +);
  RTEST (v16hf, _Float16, 16, add, +);
  RTEST (v32hf, _Float16, 32, add, +);
  RTEST (v8hf, _Float16, 8, sub, -);
  RTEST (v16hf, _Float16, 16, sub, -);
  RTEST (v32hf, _Float16, 32, sub, -);
  RTEST (v8hf, _Float16, 8, mul, *);
  RTEST (v16hf, _Float16, 16, mul, *);
  RTEST (v32hf, _Float16, 32, mul, *);
  RTEST (v8hf, _Float16, 8, div, /);
  RTEST (v16hf, _Float16, 16, div, /);
  RTEST (v32hf, _Float16, 32, div, /);
}

void
test_128 (void)
{
}
