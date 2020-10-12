/* PR target/96939 */
/* { dg-options "-march=armv8-a+crc" } */

#include <arm_acle.h>

unsigned
crc (unsigned x, const void *y)
{
  return __crc32cw (x, *(unsigned *) y);
}
