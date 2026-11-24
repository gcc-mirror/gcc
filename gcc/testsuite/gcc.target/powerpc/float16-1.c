/* { dg-do run } */
/* { dg-require-effective-target ppc_float16_runtime } */
/* { dg-options "-mfloat16 -O2" } */

#include <stdlib.h>

/* This tests whether we can do _Float16 calculations either with software
   emuluation or via hardware support.  */
volatile _Float16 two = 2.0F16;
volatile _Float16 three = 3.0F16;
volatile _Float16 result_add, result_sub, result_mul, result_div;

int
main (int argc, char *argv[])
{
  result_add = three + two;
  result_sub = three - two;
  result_mul = three * two;
  result_div = three / two;

  if (((double)result_add) != 5.0)
    abort ();

  if (((double)result_sub) != 1.0)
    abort ();

  if (((double)result_mul) != 6.0)
    abort ();

  if (((double)result_div) != 1.5)
    abort ();

  return 0;
}
