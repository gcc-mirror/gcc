/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */

/* Ensure the smlatb doesn't get generated when reading the Q flag
   from ACLE.  */

#include <arm_acle.h>

int
foo (int x, int in, int32_t c)
{
  short a = in & 0xffff;
  short b = (in & 0xffff0000) >> 16;
  
  int res =  x + b * a + __ssat (c, 24);
  return res + __saturation_occurred ();
}

/* { dg-final { scan-assembler-not "smlatb\\t" } } */
