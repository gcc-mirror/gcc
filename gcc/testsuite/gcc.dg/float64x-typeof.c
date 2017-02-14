/* Test _Float64x constant types.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

#define WIDTH 64
#define EXT 1
#include "floatn-typeof.h"
