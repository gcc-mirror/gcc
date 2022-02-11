/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

int64x2_t f1(int64_t *ptr) {
  int64_t x = *ptr;
  asm volatile ("" ::: "memory");
  if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    return (int64x2_t) { 0, x };
  else
    return (int64x2_t) { x, 0 };
}

/* { dg-final { scan-assembler {\tldr\td0, \[x0\]\n} } } */
