/* PR target/96939 */
/* { dg-options "-mcpu=unset -march=armv8-a+simd+crc -mfpu=auto" } */

#include <arm_acle.h>

unsigned
crc (unsigned x, const void *y)
{
  return __crc32cw (x, *(unsigned *) y);
}
