/* { dg-do run } */
/* { dg-options "-std=gnu99" } */
#include <math.h>
#include <stdlib.h>

int main()
{
  /* In gnuXY mode, the size of float_t and FLT_EVAL_METHOD must
     match, with the historic exception of permitting double and 0. */
  if (sizeof(float_t) == sizeof(float) && __FLT_EVAL_METHOD__ == 1)
    abort();
  return 0;
}
