/* VEC_PERM_EXPR merges with a result wider than the inputs.  */
/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <arm_sve.h>

int32_t p[4] = { 1, 2, 3, 4 };
int32_t xs[8] = { 11, 12, 13, 14, 15, 16, 17, 18 };
int32_t r[8];

/* c = VEC_PERM <a, a>, d = VEC_PERM <c, c>.  */
void __attribute__((noipa))
f1 (int32_t *p)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  svst1_s32 (svptrue_b8 (), r, svrev_s32 (c));
}

/* c = VEC_PERM <a, a>, d = VEC_PERM <c, x>.  */
void __attribute__((noipa))
f2 (int32_t *p, svint32_t x)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  svst1_s32 (svptrue_b8 (), r, svtrn2_s32 (c, x));
}

static void
check (const int32_t *want)
{
  for (int i = 0; i < 8; i++)
    if (r[i] != want[i])
      __builtin_abort ();
}

int
main (void)
{
  static const int32_t want1[8] = { 4, 3, 2, 1, 4, 3, 2, 1 };
  static const int32_t want2[8] = { 2, 12, 4, 14, 2, 16, 4, 18 };

  f1 (p);
  check (want1);
  f2 (p, svld1_s32 (svptrue_b8 (), xs));
  check (want2);
  return 0;
}
