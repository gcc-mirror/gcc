/* Test _Float32x built-in functions.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32x_runtime } */
/* { dg-require-effective-target double_float32xplus } */

#define WIDTH 32
#define EXT 1
#include "floatn-builtin.h"
