/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mprefer-vector-width=512" } */

/* { dg-final { scan-assembler-times "vcmpph" 27 } } */
/* { dg-final { scan-assembler-times "(?:vpcmpw|vpcmpeqw)" 12 } } */
/* { dg-final { scan-assembler-times "vpcmpuw" 6 } } */

typedef unsigned short u16;
typedef short s16;

#define CONDMOV_LOOP(size, type, ptype, op, name) \
void \
__attribute__ ((noinline, noclone, optimize("tree-vectorize"))) \
loop_cond_##size##ptype##type##name ( \
  ptype * restrict a, ptype * restrict b,	\
  type * restrict c, type * restrict d) \
{ \
  int i;  \
  for (i = 0; i < size; i++)  \
    { \
      if (a[i] op b[i])	\
	d[i] = c[i];  \
    } \
}

CONDMOV_LOOP (32, _Float16, _Float16, <, lt)
CONDMOV_LOOP (32, _Float16, _Float16, >, gt)
CONDMOV_LOOP (32, _Float16, _Float16, ==, eq)
CONDMOV_LOOP (16, _Float16, _Float16, <, lt)
CONDMOV_LOOP (16, _Float16, _Float16, >, gt)
CONDMOV_LOOP (16, _Float16, _Float16, ==, eq)
CONDMOV_LOOP (8, _Float16, _Float16, <, lt)
CONDMOV_LOOP (8, _Float16, _Float16, >, gt)
CONDMOV_LOOP (8, _Float16, _Float16, ==, eq)
CONDMOV_LOOP (32, _Float16, s16, <, lt)
CONDMOV_LOOP (32, _Float16, s16, >, gt)
CONDMOV_LOOP (32, _Float16, s16, ==, eq)
CONDMOV_LOOP (16, _Float16, s16, <, lt)
CONDMOV_LOOP (16, _Float16, s16, >, gt)
CONDMOV_LOOP (16, _Float16, s16, ==, eq)
CONDMOV_LOOP (8, _Float16, s16, <, lt)
CONDMOV_LOOP (8, _Float16, s16, >, gt)
CONDMOV_LOOP (8, _Float16, s16, ==, eq)
CONDMOV_LOOP (32, s16, _Float16, <, lt)
CONDMOV_LOOP (32, s16, _Float16, >, gt)
CONDMOV_LOOP (32, s16, _Float16, ==, eq)
CONDMOV_LOOP (16, s16, _Float16, <, lt)
CONDMOV_LOOP (16, s16, _Float16, >, gt)
CONDMOV_LOOP (16, s16, _Float16, ==, eq)
CONDMOV_LOOP (8, s16, _Float16, <, lt)
CONDMOV_LOOP (8, s16, _Float16, >, gt)
CONDMOV_LOOP (8, s16, _Float16, ==, eq)
CONDMOV_LOOP (32, _Float16, u16, <, lt)
CONDMOV_LOOP (32, _Float16, u16, >, gt)
CONDMOV_LOOP (32, _Float16, u16, ==, eq)
CONDMOV_LOOP (16, _Float16, u16, <, lt)
CONDMOV_LOOP (16, _Float16, u16, >, gt)
CONDMOV_LOOP (16, _Float16, u16, ==, eq)
CONDMOV_LOOP (8, _Float16, u16, <, lt)
CONDMOV_LOOP (8, _Float16, u16, >, gt)
CONDMOV_LOOP (8, _Float16, u16, ==, eq)
CONDMOV_LOOP (32, u16, _Float16, <, lt)
CONDMOV_LOOP (32, u16, _Float16, >, gt)
CONDMOV_LOOP (32, u16, _Float16, ==, eq)
CONDMOV_LOOP (16, u16, _Float16, <, lt)
CONDMOV_LOOP (16, u16, _Float16, >, gt)
CONDMOV_LOOP (16, u16, _Float16, ==, eq)
CONDMOV_LOOP (8, u16, _Float16, <, lt)
CONDMOV_LOOP (8, u16, _Float16, >, gt)
CONDMOV_LOOP (8, u16, _Float16, ==, eq)
