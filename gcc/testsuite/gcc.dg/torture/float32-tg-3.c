/* Test _Float32 type-generic built-in functions: __builtin_fpclassify.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32_runtime } */
/* { dg-skip-if "No subnormal support" { csky-*-* } { "-mhard-float" } } */

#define WIDTH 32
#define EXT 0
#include "floatn-tg-3.h"
