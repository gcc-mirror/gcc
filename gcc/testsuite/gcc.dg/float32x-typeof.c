/* Test _Float32x constant types.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32x } */

#define WIDTH 32
#define EXT 1
#include "floatn-typeof.h"
