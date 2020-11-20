/* PR target/97528 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1" }  */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

typedef __simd64_int16_t T;
typedef __simd64_uint16_t U;
unsigned short c;
int d;
U e;

void
foo (void)
{
  unsigned short *dst = &c;
  int g = d, b = 4;
  U dc = e;
  for (int h = 0; h < b; h++)
    {
      unsigned short *i = dst;
      U j = dc;
      vst1_s16 ((int16_t *) i, (T) j);
      dst += g;
    }
}
