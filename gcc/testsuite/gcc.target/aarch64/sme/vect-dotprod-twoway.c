/* { dg-additional-options "-O2 -ftree-vectorize" } */

#include <stdint.h>
#pragma GCC target "+sme2"

uint32_t udot2(int n, uint16_t* data) __arm_streaming
{
  uint32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

int32_t sdot2(int n, int16_t* data) __arm_streaming
{
  int32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

/* { dg-final { scan-assembler-times {\tudot\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h\n} 5 } } */
/* { dg-final { scan-assembler-times {\tsdot\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h\n} 5 } } */
/* { dg-final { scan-assembler-times {\twhilelo\t} 4 } } */
