/* Test _Float128 NaNs in <float.h>.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -fsignaling-nans" } */
/* { dg-add-options float128 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float128_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 128
#define EXT 0
#include "floatn-nan-floath.h"
