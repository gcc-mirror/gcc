/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -fdump-tree-optimized" } */

/* PR tree-optimization/117234  */
/* VEC_DUPLICATE_EXPR should not be declared as trapping.  */
#include <arm_sve.h>

svfloat32_t f(float a)
{
  try {
    return svdup_f32(a);
  }  catch(...)
  { __builtin_trap (); }
}

/* { dg-final { scan-tree-dump-not "__builtin_trap" "optimized" } } */
