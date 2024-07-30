/* { dg-do compile } */
/* { dg-options "-msse2 -mfancy-math-387 -mfpmath=sse" } */

#include <float.h>

#if FLT_EVAL_METHOD != 0
# error FLT_EVAL_METHOD != 0
#endif
