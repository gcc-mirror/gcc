/* { dg-options "-O" } */

#pragma GCC aarch64 "arm_neon.h"
typedef __Int64x1_t int64x1_t;
void foo (int64x1x4_t);

void
bar (int64x1_t a)
{
  for (;;) {
    int64x1x4_t b;
    b.val[3] = a;
    foo (b);
  }
}
