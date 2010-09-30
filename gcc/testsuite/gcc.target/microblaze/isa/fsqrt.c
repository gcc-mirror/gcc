/* { dg-options "-O3 -mcpu=v6.00.a -mhard-float -mxl-float-sqrt" } */
#include <math.h>

float sqrt_func (float f) 
{
  /* { dg-final { scan-assembler "fsqrt\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),r(\[0-9]\|\[1-2]\[0-9]\|3\[0-1])\[^0-9]" } } */
    return sqrtf (f);
}


