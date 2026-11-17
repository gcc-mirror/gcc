/* { dg-do compile { target { aarch64*-*-* } } } */

#include <arm_neon.h>

int8_t *bar();

void foo() {
  vld1q_u8 (bar ());
}
