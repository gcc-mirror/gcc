/* Test _Float64 constant types.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64 } */

#define WIDTH 64
#define EXT 0
#include "floatn-typeof.h"
