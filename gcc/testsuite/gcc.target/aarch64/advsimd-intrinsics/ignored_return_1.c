/* { dg-do compile { target { aarch64*-*-* } } } */

#include <arm_neon.h>

int8_t *bar();

void foo() {
  __builtin_aarch64_ld1v16qi(bar());
}
