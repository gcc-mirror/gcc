/* PR target/68674 */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=generic+nosimd" } */

#include <arm_neon.h>

int8x8_t a;
extern int8x8_t b;
int16x8_t e;

void __attribute__((target("+simd")))
foo1(void)
{
  e = (int16x8_t) vaddl_s8(a, b);
}

int8x8_t __attribute__((target("+simd")))
foo2(void)
{
  return a;
}

