#include <arm_neon.h>
#include <arm_sve.h>

void
f (void)
{
  {
    register svint8_t v0 asm ("z0");
    asm volatile ("" : "=w" (v0));
  }
  {
    register int8x8x4_t v0 asm ("v0");
    asm volatile ("" : "=w" (v0));
  }
}
