/* { dg-do run } */
/* { dg-options "-std=c11" } */

/* Test that when compiling with -std=c11 and defining
   __STDC_WANT_IEC_60559_TYPES_EXT__, we only see the ISO/IEC TS
   18661-3 values for FLT_EVAL_METHOD.  */

#define __STDC_WANT_IEC_60559_TYPES_EXT__

#include <float.h>

int main (int argc, char** argv)
{
  switch (__FLT_EVAL_METHOD__)
    {
      case 0:
      case 1:
      case 2:
      case 16:
      case -1:
	return 0;
      default:
	return 1;
    }
}
