/* Test _Float32x.  */
/* { dg-do run } */
/* { dg-options "-Wno-old-style-definition" } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32x_runtime } */

#define WIDTH 32
#define EXT 1
#include "floatn-basic.h"
