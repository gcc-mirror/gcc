/* Test _Float32x NaNs in <float.h>.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -fsignaling-nans" } */
/* { dg-add-options float32x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32x_runtime } */
/* { dg-require-effective-target fenv_exceptions_double } */

#define WIDTH 32
#define EXT 1
#include "floatn-nan-floath.h"
