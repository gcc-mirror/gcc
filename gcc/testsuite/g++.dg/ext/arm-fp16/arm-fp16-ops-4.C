/* Test various operators on __fp16 and mixed __fp16/float operands.  */
/* { dg-do run { target arm*-*-* } } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfp16-format=alternative -ffast-math" } */

#include "arm-fp16-ops.h"
