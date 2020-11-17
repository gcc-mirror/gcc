/* Test _Float32 NaNs in <float.h>.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -fsignaling-nans" } */
/* { dg-add-options float32 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 32
#define EXT 0
#include "floatn-nan-floath.h"
