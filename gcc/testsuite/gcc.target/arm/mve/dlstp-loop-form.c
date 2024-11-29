/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-options "-Ofast -std=c99" } */
/* { dg-add-options arm_v8_1m_mve_fp } */
#pragma GCC arm "arm_mve_types.h"
#pragma GCC arm "arm_mve.h" false
typedef __attribute__((aligned(2))) float16x8_t e;
mve_pred16_t c(long d) { return __builtin_mve_vctp16qv8bi(d); }
int f();
void n() {
  int g, h, *i, j;
  mve_pred16_t k;
  e acc;
  e l;
  e m;
  for (;;) {
    j = g;
    acc[g];
    for (; h < g; h += 8) {
      k = c(j);
      acc = vfmsq_m(acc, l, m, k);
      j -= 8;
    }
    i[g] = f(acc);
  }
}

int a;
void g2() {
  long b;
  while (a) {
    char *c;
    for (long d = b; d > 0; d -= 4) {
      mve_pred16_t e = vctp32q(d);
      int32x4_t f;
      vstrbq_p_s32(c, f, e);
    }
  }
}
