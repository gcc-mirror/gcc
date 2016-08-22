/* Test _Float32 NaNs.  */
/* { dg-do run } */
/* { dg-options "-fsignaling-nans" } */
/* { dg-add-options float32 } */
/* { dg-require-effective-target float32_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 32
#define EXT 0
#include "floatn-nan.h"
