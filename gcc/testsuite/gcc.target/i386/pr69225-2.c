/* { dg-do compile } */
/* { dg-options "-mno-sse -mno-80387" } */

#include <float.h>

#if FLT_EVAL_METHOD != 0
# error FLT_EVAL_METHOD != 0
#endif
