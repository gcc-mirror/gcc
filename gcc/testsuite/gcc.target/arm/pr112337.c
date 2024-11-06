/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */

#pragma GCC arm "arm_mve_types.h"
#pragma GCC arm "arm_mve.h" false

int32x4_t h(void *p) { return vldrwq_s32(p); }
void g(int32x4_t);
void f(int, int, int, short, int *p) {
  int *bias = p;
  for (;;) {
    int32x4_t d = h(bias);
    bias += 4;
    g(d);
  }
}
