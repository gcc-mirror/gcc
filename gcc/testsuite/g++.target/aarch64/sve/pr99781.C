/* { dg-do compile } */
/* { dg-options "-march=armv8-a+sve" } */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size(32)));

void
foo (int32_t val)
{
  register vnx4si x asm ("z0");
  register vnx4si y asm ("z1");
  asm volatile ("" : "=w" (y));
  val += 1;
  vnx4si z = { val, val, val, val, val, val, val, val };
  x = (vnx4si) { -1, 0, 0, -1, 0, -1, 0, -1 } ? z : y;
  asm volatile ("" :: "w" (x));
}
