/* { dg-skip-if "requires hosted libstdc++ for cmath" { ! hostedlib } } */

#include <math.h>

int test_1 (void)
{
  float foo = 42.;
  if (isnan (foo))
    return 1;
  return 0;
}
