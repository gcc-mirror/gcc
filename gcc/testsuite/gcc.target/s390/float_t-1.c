/* { dg-do run } */
/* { dg-options "-std=c99" } */
#include <math.h>
#include <stdlib.h>

int main()
{
  /* In standard-compliant mode, the size of float_t and FLT_EVAL_METHOD must
     match. */
  if (sizeof(float_t) == sizeof(double) && __FLT_EVAL_METHOD__ != 1)
    abort();
  if (sizeof(float_t) == sizeof(float) && __FLT_EVAL_METHOD__ != 0)
    abort();
  return 0;
}
