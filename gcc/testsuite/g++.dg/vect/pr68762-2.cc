// PR middle-end/68762
// { dg-do compile }

#include "pr68762.h"

#pragma omp declare simd
double
baz (double x)
{
  return x;
}

double
fn (double x)
{
  return foo (x);
}
