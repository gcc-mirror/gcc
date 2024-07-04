/* Test _Float64x NaNs in <float.h>.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -fsignaling-nans" } */
/* { dg-add-options float64x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float64x_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 64
#define EXT 1
#include "floatn-nan-floath.h"
