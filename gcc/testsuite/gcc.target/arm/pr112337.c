/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.1-m.main+fp.dp+mve.fp -mfloat-abi=hard" } */

#pragma GCC arm "arm_mve_types.h"
int32x4_t h(void *p) { return __builtin_mve_vldrwq_sv4si(p); }
void g(int32x4_t);
void f(int, int, int, short, int *p) {
  int *bias = p;
  for (;;) {
    int32x4_t d = h(bias);
    bias += 4;
    g(d);
  }
}
