/* { dg-do run } */
/* { dg-options "-std=c11" } */

/* Test that when compiling with -std=c11, we only see the C99/C11 values
   for FLT_EVAL_METHOD.  */

#include <float.h>

int main (int argc, char** argv)
{
  switch (FLT_EVAL_METHOD)
    {
      case 0:
      case 1:
      case 2:
      case -1:
	return 0;
      default:
	return 1;
    }
}
