/* Test specification of custom instructions via pragma in the presence
   of LTO.  This test case formerly failed due to PR60179.  */

/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O1 -flto -flto-partition=one -save-temps" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <stdio.h> 
#include <math.h>

#pragma GCC target ("custom-fabss=224")

float
custom_fp (float operand_a)
{
  return fabsf (operand_a);
}

int
main (int argc, char *argv[])
{
  return custom_fp ((float)argc) > 1.0;
}

/* { dg-final { scan-lto-assembler "custom\\t224, " } } */
/* { dg-final { cleanup-saved-temps } } */
