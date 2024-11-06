/* Test _Float32.  */
/* { dg-do run } */
/* { dg-options "-Wno-old-style-definition" } */
/* { dg-add-options float32 } */
/* { dg-require-effective-target float32_runtime } */

#define WIDTH 32
#define EXT 0
#include "floatn-basic.h"
