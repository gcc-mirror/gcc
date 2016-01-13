/* { dg-do compile} */
/* { dg-options "-msse -mno-sse2 -m80387 -mfpmath=sse" } */

#include <float.h>

#if FLT_EVAL_METHOD != -1
# error FLT_EVAL_METHOD != -1
#endif
