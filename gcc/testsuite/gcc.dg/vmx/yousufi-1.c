#include "harness.h"

/* Tests the vec_ctu function, which converts a vector of floats to a vector
   of unsigned ints.  In powerpc-eabisim-run ver. moto-1.0, vec_ctu produces
   strange output for input values of less than ~.0039. -Umair */

static void test()
{
  vector float input = ((vector float){0.003,0.003,0.003,0.003});
  vector unsigned int output;
  vector unsigned int expect = ((vector unsigned int){0,0,0,0});

  output = vec_ctu(input, 1);
  check(vec_all_eq(output, expect), "vec_ctu");
}
