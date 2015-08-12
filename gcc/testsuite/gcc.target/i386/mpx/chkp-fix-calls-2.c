/* { dg-do compile } */
/* { dg-options "-O3 -fcheck-pointer-bounds -mmpx -fno-inline" } */

#include "math.h"

double
test1 (double x, double y, double (*fn)(double, double))
{
  return fn (x, y);
}

double
test2 (double x, double y)
{
  return test1 (x, y, copysign);
}
