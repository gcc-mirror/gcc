/* Test _Float64 NaNs in <float.h>.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -fsignaling-nans" } */
/* { dg-add-options float64 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float64_runtime } */
/* { dg-require-effective-target fenv_exceptions_double } */

#define WIDTH 64
#define EXT 0
#include "floatn-nan-floath.h"
