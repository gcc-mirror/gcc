/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx" } */

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
