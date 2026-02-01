/* { dg-do compile } */
/* { dg-options "-O1" } */
/* PR rtl-optimization/123294 */

#include <arm_neon.h>

typedef unsigned a;
int b;
a d() {
  __attribute__((__vector_size__(4 * sizeof(a)))) a e[5];
  if (0 > b) {
    __attribute__((__vector_size__(8 * sizeof(a)))) a f = {b, b};
    e[0] = __builtin_shufflevector(f, f, 0, 1, 2, 3);
  }
  while (vqadd_u64((uint64x1_t){3}, (uint64x1_t){0})[0])
    ;
  return e[0][3];
}
