/* Test _Float16.  */
/* { dg-do run } */
/* { dg-options "-Wno-old-style-definition" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16_runtime } */

#define WIDTH 16
#define EXT 0
#include "floatn-basic.h"
