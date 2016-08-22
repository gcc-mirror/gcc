/* Test _Float64 NaNs.  */
/* { dg-do run } */
/* { dg-options "-fsignaling-nans" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 64
#define EXT 0
#include "floatn-nan.h"
