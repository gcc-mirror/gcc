/* { dg-do compile } */
/* { dg-additional-options "-w" } */

#include <stdint.h>

int a, b, c;
void d() {
  uint16_t e;
  int32_t *f;
  int32_t *g;
  if (a) {
    int32_t *k;
    for (;; *k += 1) {
      int32_t **i = &f;
      int32_t **l = &g;
      for (e = 6; e; e++) {
        g = k = f;
      j:
        **l = 0;
      }
      *i = (int32_t *) c;
    }
  }
  uint16_t i = (uint16_t) &e;
  b = i / 0;
  goto j;
}
