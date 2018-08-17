/* Test _Float64x type-generic built-in functions: __builtin_fpclassify.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float64x_runtime } */
/* { dg-skip-if "No subnormal support" { csky-*-* } { "-mhard-float" } } */

#define WIDTH 64
#define EXT 1
#include "floatn-tg-3.h"
