/* Test _Float64 type-generic built-in functions: __builtin_fpclassify.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float64_runtime } */
/* { dg-skip-if "No subnormal support" { csky-*-* } { "-mhard-float" } } */

#define WIDTH 64
#define EXT 0
#include "floatn-tg-3.h"
