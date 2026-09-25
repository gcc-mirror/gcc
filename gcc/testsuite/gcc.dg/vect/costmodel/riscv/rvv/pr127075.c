/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=conv-dynamic" } */

/* Ensure we choose no larger than LMUL2. LMUL4 and LMUL8 cause spilling.  */

#include <stdint.h>

void
wide (uint8_t *__restrict a, uint8_t *__restrict b,
      uint8_t *__restrict c, uint8_t *__restrict d,
      int64_t *__restrict o0, int64_t *__restrict o1,
      int64_t *__restrict o2, int64_t *__restrict o3,
      int64_t *__restrict o4, int64_t *__restrict o5, int n)
{
  for (int i = 0; i < n; i++)
    {
      int64_t va = a[i], vb = b[i], vc = c[i], vd = d[i];
      int64_t t0 = va + vb, t1 = vb + vc, t2 = vc + vd, t3 = vd + va;
      int64_t t4 = va * vb, t5 = vb * vc, t6 = vc * vd, t7 = vd * va;
      o0[i] = t0 + t4;
      o1[i] = t1 + t5;
      o2[i] = t2 + t6;
      o3[i] = t3 + t7;
      o4[i] = t0 * t1 + t2 * t3;
      o5[i] = t4 * t5 + t6 * t7;
    }
}

/* { dg-final { scan-assembler-not "vs\[1248\]r" } } */
