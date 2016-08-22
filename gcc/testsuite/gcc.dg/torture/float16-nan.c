/* Test _Float16 NaNs.  */
/* { dg-do run } */
/* { dg-options "-fsignaling-nans" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16_runtime } */
/* { dg-require-effective-target fenv_exceptions } */

#define WIDTH 16
#define EXT 0
#include "floatn-nan.h"
