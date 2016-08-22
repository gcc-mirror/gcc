/* Test _Float64x built-in functions.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x_runtime } */

#define WIDTH 64
#define EXT 1
#include "floatn-builtin.h"
