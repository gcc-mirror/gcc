/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mprefer-vector-width=512" } */

static void condmov_test (void);
#define DO_TEST condmov_test
#define AVX512FP16
#define AVX512VL
#include "avx512f-check.h"
#include "avx512fp16-vcondmn-loop-1.c"

_Float16 a[32], b[32], c[32], fexp[32], fref[32];
s16 sa[32], sb[32], sc[32], sexp[32], sref[32];
u16 ua[32], ub[32], uc[32], uexp[32], uref[32];

#define EMULATE_CONDMOV_LOOP(size, type, ptype, op, name) \
void \
__attribute__ ((noinline, noclone)) \
scalar_cond_##size##ptype##type##name ( \
  ptype * restrict a, ptype * restrict b,	\
  type * restrict c, type * restrict d)  \
{ \
  int i;  \
  for (i = 0; i < size; i++)  \
    { \
      if (a[i] op b[i])	\
	d[i] = c[i];  \
    } \
}

EMULATE_CONDMOV_LOOP (32, _Float16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (32, _Float16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (32, _Float16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (16, _Float16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (16, _Float16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (16, _Float16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (8, _Float16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (8, _Float16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (8, _Float16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (32, _Float16, s16, <, lt)
EMULATE_CONDMOV_LOOP (32, _Float16, s16, >, gt)
EMULATE_CONDMOV_LOOP (32, _Float16, s16, ==, eq)
EMULATE_CONDMOV_LOOP (16, _Float16, s16, <, lt)
EMULATE_CONDMOV_LOOP (16, _Float16, s16, >, gt)
EMULATE_CONDMOV_LOOP (16, _Float16, s16, ==, eq)
EMULATE_CONDMOV_LOOP (8, _Float16, s16, <, lt)
EMULATE_CONDMOV_LOOP (8, _Float16, s16, >, gt)
EMULATE_CONDMOV_LOOP (8, _Float16, s16, ==, eq)
EMULATE_CONDMOV_LOOP (32, s16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (32, s16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (32, s16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (16, s16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (16, s16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (16, s16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (8, s16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (8, s16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (8, s16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (32, _Float16, u16, <, lt)
EMULATE_CONDMOV_LOOP (32, _Float16, u16, >, gt)
EMULATE_CONDMOV_LOOP (32, _Float16, u16, ==, eq)
EMULATE_CONDMOV_LOOP (16, _Float16, u16, <, lt)
EMULATE_CONDMOV_LOOP (16, _Float16, u16, >, gt)
EMULATE_CONDMOV_LOOP (16, _Float16, u16, ==, eq)
EMULATE_CONDMOV_LOOP (8, _Float16, u16, <, lt)
EMULATE_CONDMOV_LOOP (8, _Float16, u16, >, gt)
EMULATE_CONDMOV_LOOP (8, _Float16, u16, ==, eq)
EMULATE_CONDMOV_LOOP (32, u16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (32, u16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (32, u16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (16, u16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (16, u16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (16, u16, _Float16, ==, eq)
EMULATE_CONDMOV_LOOP (8, u16, _Float16, <, lt)
EMULATE_CONDMOV_LOOP (8, u16, _Float16, >, gt)
EMULATE_CONDMOV_LOOP (8, u16, _Float16, ==, eq)

void init()
{
  int i;
  for (i = 0; i < 32; i++)
    {
      ua[i] = sa[i] = a[i] = i; 
      ub[i] = sb[i] = b[i] = i;
      uc[i] = sc[i] = c[i] = (32 - i) * 2;
      uexp[i] = sexp[i] = fexp[i] = -1;
      uref[i] = sref[i] = fref[i] = -1;
    }
}

int check_cond(void *a, void *b, int size)
{
  int i;
  u16 *pa = (u16 *)a, *pb = (u16 *)b;
  for (i = 0; i < size; i++)
    if (pa[i] != pb[i])
      return 0;
  return 1;
}

#define TEST_CONDMOV_LOOP(size, name)	\
{ \
  init ();  \
  scalar_cond_##size##_Float16_Float16##name (a, b, c, fexp);  \
  loop_cond_##size##_Float16_Float16##name (a, b, c, fref);  \
  if (!check_cond ((void *)fexp, (void *)fref, size)) \
    abort();  \
  \
  init ();  \
  scalar_cond_##size##_Float16s16##name (a, b, sc, sexp);  \
  loop_cond_##size##_Float16s16##name (a, b, sc, sref);  \
  if (!check_cond ((void *)sexp, (void *)sref, size)) \
    abort();  \
  \
  init ();  \
  scalar_cond_##size##s16_Float16##name (sa, sb, c, fexp);  \
  loop_cond_##size##s16_Float16##name (sa, sb, c, fref);  \
  if (!check_cond ((void *)fexp, (void *)fref, size)) \
    abort();  \
  \
  init ();  \
  scalar_cond_##size##_Float16u16##name (a, b, uc, uexp);  \
  loop_cond_##size##_Float16u16##name (a, b, uc, uref);  \
  if (!check_cond ((void *)uexp, (void *)uref, size)) \
    abort();  \
  \
  init ();  \
  scalar_cond_##size##u16_Float16##name (ua, ub, c, fexp);  \
  loop_cond_##size##u16_Float16##name (ua, ub, c, fref);  \
  if (!check_cond ((void *)fexp, (void *)fref, size)) \
    abort();  \
}

static void condmov_test()
{
  TEST_CONDMOV_LOOP (32, lt)
  TEST_CONDMOV_LOOP (32, gt)
  TEST_CONDMOV_LOOP (32, eq)
  TEST_CONDMOV_LOOP (16, lt)
  TEST_CONDMOV_LOOP (16, gt)
  TEST_CONDMOV_LOOP (16, eq)
  TEST_CONDMOV_LOOP (8, lt)
  TEST_CONDMOV_LOOP (8, gt)
  TEST_CONDMOV_LOOP (8, eq)
}
