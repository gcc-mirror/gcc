/* { dg-do compile } */
/* { dg-options "-msse2 -m80387 -mfpmath=sse,387" } */

#include <float.h>

#if FLT_EVAL_METHOD != -1
# error FLT_EVAL_METHOD != -1
#endif
