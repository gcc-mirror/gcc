/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
#include <arm_mve.h>

void g(int32x4_t);
void f(int, int, int, short, int32_t *p) {
  int32_t *bias = p;
  for (;;) {
    int32x4_t d = vldrwq_s32 (p);
    bias += 4;
    g(d);
  }
}
