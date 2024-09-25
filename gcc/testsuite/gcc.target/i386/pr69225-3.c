/* { dg-do compile } */
/* { dg-options "-m80387 -mfpmath=387" } */

#include <float.h>

#if FLT_EVAL_METHOD != 2
# error FLT_EVAL_METHOD != 2
#endif
