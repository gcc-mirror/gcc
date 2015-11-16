/* { dg-lto-do run } */
/* { dg-lto-options {{-flto -mfpu=neon}} } */
/* { dg-suppress-ld-options {-mfpu=neon} } */

#include "arm_neon.h"

float32x2_t a, b, c, e;

int main()
{
  e = __builtin_neon_vmls_lanev2sf (a, b, c, 0);
  return 0;
}

