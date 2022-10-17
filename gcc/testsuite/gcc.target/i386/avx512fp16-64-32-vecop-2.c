/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

static void vec_op_test (void);
#define DO_TEST vec_op_test
#define AVX512FP16
#define AVX512VL
#include "avx512f-check.h"
#include "avx512fp16-64-32-vecop-1.c"

_Float16 a[4], b[4], fexp[4], fref[4];

#define EMULATE_VEC_OP_VV(size, op, name) \
void \
__attribute__ ((noinline, noclone)) \
scalar_vecop_v##size##hf##name ( \
  _Float16 * restrict dst, _Float16 * restrict src1,  \
  _Float16 * restrict src2)  \
{ \
  int i;  \
  for (i = 0; i < size; i++)  \
    dst[i] = src1[i] op src2[i];  \
}

EMULATE_VEC_OP_VV (4, +, add)
EMULATE_VEC_OP_VV (2, +, add)
EMULATE_VEC_OP_VV (4, -, sub)
EMULATE_VEC_OP_VV (2, -, sub)
EMULATE_VEC_OP_VV (4, *, mul)
EMULATE_VEC_OP_VV (2, *, mul)
EMULATE_VEC_OP_VV (4, /, div)
EMULATE_VEC_OP_VV (2, /, div)

void init()
{
  int i;
  for (i = 0; i < 4; i++)
    {
      a[i] = i + 0.5; 
      b[i] = i * 1.5;
      fexp[i] = fref[i] = 2.75 * i;
    }
}

int check_cond(void *a, void *b, int size)
{
  int i;
  unsigned short *pa = (unsigned short *)a,
		 *pb = (unsigned short *)b;
  for (i = 0; i < size; i++)
    if (pa[i] != pb[i])
      return 0;
  return 1;
}

#define TEST_VEC_OP_VV(size, name)	\
{ \
  init ();  \
  scalar_vecop_v##size##hf##name (a, b, fexp);  \
  vecop_v##size##hf##name (a, b, fref);  \
  if (!check_cond ((void *)fexp, (void *)fref, size)) \
    abort();  \
}

static void vec_op_test()
{
  TEST_VEC_OP_VV (4, add)
  TEST_VEC_OP_VV (2, add)
  TEST_VEC_OP_VV (4, sub)
  TEST_VEC_OP_VV (2, sub)
  TEST_VEC_OP_VV (4, mul)
  TEST_VEC_OP_VV (2, mul)
  TEST_VEC_OP_VV (4, div)
  TEST_VEC_OP_VV (2, div)
}
