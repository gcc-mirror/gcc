/* Test _Float128 NaNs.  */
/* { dg-do run } */
/* { dg-options "-fsignaling-nans" } */
/* { dg-add-options float128 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float128_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 128
#define EXT 0
#include "floatn-nan.h"
