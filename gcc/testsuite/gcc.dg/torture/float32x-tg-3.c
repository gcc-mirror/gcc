/* Test _Float32x type-generic built-in functions: __builtin_fpclassify.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32x_runtime } */
/* { dg-skip-if "No subnormal support" { csky-*-* } { "-mhard-float" } } */

#define WIDTH 32
#define EXT 1
#include "floatn-tg-3.h"
