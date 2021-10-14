/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

int32_t f (int32_t *restrict array, int len, int min)
{
  int32_t iSum = 0;

  for (int i=0; i<len; i++) {
    if (array[i] >= min)
       iSum += array[i];
  }
  return iSum;
}


/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+\.s, p1, z[0-9]+\.s, z[0-9]+\.s} } } */
